package com.olegpy.schive

import java.nio.ByteBuffer
import java.nio.channels.SeekableByteChannel
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.util.Try

import net.sf.sevenzipjbinding._
import net.sf.sevenzipjbinding.impl.OutItemFactory
import net.sf.sevenzipjbinding.simple.ISimpleInArchiveItem
import net.sf.sevenzipjbinding.util.ByteArrayStream


class Archive private (path: Path) {
  import Archive._

  lazy val files = entries.filterNot(_._isDir)

  private def inArchive = SevenZip.openInArchive(null, new ChannelInput(Files.newByteChannel(path)))

  private def outArchive = {
    if (Files.exists(path)) {
      val inArch = inArchive
      (inArch, Left(inArch.getConnectedOutArchive))
    } else {
      val pathString = path.toString
      val ext = pathString.substring(pathString.lastIndexOf('.') + 1)
      val format = ext match {
        case "zip" => ArchiveFormat.ZIP
        case "7z"  => ArchiveFormat.SEVEN_ZIP
        case "tar" => ArchiveFormat.TAR
        case "bz2" => ArchiveFormat.BZIP2
        case "gz"  => ArchiveFormat.GZIP
        case _ => throw new IllegalArgumentException(s"Unknown archive type: $ext")
      }
      Files.createDirectories(path.getParent)
      Files.createFile(path)
      val arch = SevenZip.openOutArchive(format)
      (arch, Right(arch))
    }
  }

  private val entries: Vector[Entry] =
    if (!Files.exists(path)) Vector.empty
    else {
      val archive = inArchive
      val result: Vector[Entry] =
        archive
          .getSimpleInterface
          .getArchiveItems
          .map(i => new Entry(i))(collection.breakOut)

      archive.close()
      result
    }

  def extractSome(f: Entry => Option[Path]): Archive = {
    val output = (path: Path, bytes: Array[Byte]) => {
      if (!Files.exists(path.getParent)) {
        Files.createDirectories(path.getParent)
      }

      Files.write(path, bytes, StandardOpenOption.CREATE)
      ()
    }

    val curried = output.curried
    val consumer = f andThen (_.map(curried))

    processSome(consumer)
  }

  def mapSome[A](f: Entry => Option[Array[Byte] => A]): Vector[A] = {
    val b = Vector.newBuilder[A]
    val push = (a: A) => { b += a; () }
    val consumer = f andThen (opt => opt map (_ andThen push))
    processSome(consumer)
    b.result()
  }

  def processSome(f: Entry => Option[Array[Byte] => Unit]) = {
    val backingArchive = inArchive
    val callback = new ExtractCallback(f, entries)

    throwIfFailed(
      Try { backingArchive.extract(null, false, callback) },
      Try { backingArchive.close() }
    )

    this
  }

  def spliceSome(choices: Map[String, Option[Array[Byte]]]): Archive = {
    val map = choices.map { case (k, v) => normalized(k) -> v }
    val existingNames: Set[String] = entries.filterNot(_._isDir).map(_.path)(collection.breakOut)

    val adds = map.collect {
      case (subpath, Some(bytes)) if !existingNames(subpath) => AddNew(subpath, bytes)
    }

    val allPaths = existingNames ++ adds.map(_.path)

    val emptyDirs = entries
      .iterator
      .filter(_._isDir)
      .map(_.path)
      .filterNot(dirName => allPaths.exists(_.startsWith(dirName)))
      .toSet

    val operations = entries.zipWithIndex.collect{
      case (e, i) if !map.contains(e.path) && !emptyDirs(e.path) =>
        RetainExisting(i)
      case (e, i) if map(e.path).isDefined && !emptyDirs(e.path) =>
        ReplaceExisting(i, map(e.path).get)
    } ++ adds

    val (closer, updater) = outArchive match {
      case (c, Left(update)) => (c, update.updateItems _)
      case (c, Right(create)) => (c, create.createArchive _)
    }

    val callback = new UpdateCallback(operations)

    throwIfFailed(
      Try { updater(new ChannelOutput(Files.newByteChannel(path, StandardOpenOption.WRITE)), operations.length, callback) },
      Try { callback.close() },
      Try { closer.close() }
    )

    new Archive(path)
  }
}

object Archive {
  final val SingletonPath: String = "???"

  class Entry private[schive] (item: ISimpleInArchiveItem) {
    val path: String = Option(item.getPath).map(normalized).getOrElse(SingletonPath)

    private[schive] val _isDir = item.isFolder
  }

  sealed trait SpliceOp
  case class RetainExisting(i: Int) extends SpliceOp
  case class ReplaceExisting(i: Int, content: Array[Byte]) extends SpliceOp
  case class AddNew(path: String, content: Array[Byte]) extends SpliceOp

  def apply(path: Path): Archive = new Archive(path)

  private def normalized(path: String) = {
    val slashed = path.replace('\\', '/')
    if (slashed startsWith "/") slashed.substring(1)
    else slashed
  }

  private[schive] trait CloseLater extends AutoCloseable {
    private[CloseLater] var toClose = List.empty[AutoCloseable]
    def closeLater[A <: AutoCloseable](a: A): A = {
      toClose ::= a
      a
    }

    override def close() = {
      throwIfFailed(toClose.map(closeable => Try(closeable.close())): _*)
    }
  }

  private[schive] def throwIfFailed(tries: Try[Any]*) =
    for {
      op <- tries
      if op.isFailure
    } op.get

  private[schive] class ChannelInput(channel: SeekableByteChannel)
    extends IInStream
       with AutoCloseable
  {
    import ISeekableStream._

    override def read(data: Array[Byte]): Int = {
      val read = channel.read(ByteBuffer.wrap(data))
      if (read == -1) 0 else read
    }

    override def seek(offset: Long, seekOrigin: Int): Long = synchronized {
      channel.position(seekOrigin match {
        case SEEK_SET => offset
        case SEEK_CUR => channel.position() + offset
        case SEEK_END => channel.size() + offset
        case _ => throw new IllegalArgumentException()
      })
      channel.position()
    }

    override def close(): Unit = channel.close()
  }

  private[schive] class ExtractCallback(f: Entry => Option[Array[Byte] => Unit], entries: Vector[Entry])
    extends IArchiveExtractCallback
  {
    private[this] var current: (ByteArrayStream, Array[Byte] => Unit) = _

    override def getStream(index: Int, extractAskMode: ExtractAskMode): ISequentialOutStream = {
      for {
        consumer <- f(entries(index))
        if extractAskMode == ExtractAskMode.EXTRACT && !entries(index)._isDir
        stream = new ByteArrayStream(Int.MaxValue)
      } yield {
        current = stream -> consumer
        stream
      }
    }.orNull

    override def prepareOperation(extractAskMode: ExtractAskMode): Unit = ()
    override def setOperationResult(result: ExtractOperationResult): Unit =
      if (result == ExtractOperationResult.OK) {
        val (stream, consumer) = current
        throwIfFailed(
          Try(consumer(stream.getBytes)),
          Try(stream.close())
        )
      }

    override def setCompleted(complete: Long): Unit = ()
    override def setTotal(total: Long): Unit = ()
  }

  private[schive] class ChannelOutput(channel: SeekableByteChannel)
    extends IOutStream
       with AutoCloseable
  {
    import ISeekableStream._

    override def setSize(newSize: Long): Unit = ()

    override def close(): Unit = channel.close()

    override def seek(offset: Long, seekOrigin: Int): Long =  synchronized {
      channel.position(seekOrigin match {
        case SEEK_SET => offset
        case SEEK_CUR => channel.position() + offset
        case SEEK_END => channel.size() + offset
        case _ => throw new IllegalArgumentException()
      })
      channel.position()
    }


    override def write(data: Array[Byte]): Int = synchronized {
      channel.write(ByteBuffer.wrap(data))
    }
  }

  private[schive] class UpdateCallback(entries: Vector[SpliceOp])
    extends IOutCreateCallback[IOutItemAllFormats]
       with CloseLater
  {

    override def getStream(index: Int): ISequentialInStream = {
      entries(index) match {
        case RetainExisting(_) => None
        case ReplaceExisting(_, buff) => Some(closeLater(new ByteArrayStream(buff, false)))
        case AddNew(_, buff) => Some(closeLater(new ByteArrayStream(buff, false)))
      }
    }.orNull

    override def getItemInformation(index: Int, outItemFactory: OutItemFactory[IOutItemAllFormats]) : IOutItemAllFormats = {
      entries(index) match {
        case RetainExisting(i) => outItemFactory.createOutItem(i)
        case ReplaceExisting(i, bytes) =>
          val oo = outItemFactory.createOutItemAndCloneProperties(i)
          oo.setUpdateIsNewData(true)
          oo.setUpdateIsNewProperties(true)
          oo.setDataSize(bytes.length.toLong)
          oo
        case AddNew(p, bytes) =>
          val oo = outItemFactory.createOutItem()
          oo.setUpdateIsNewData(true)
          if (p != SingletonPath) oo.setPropertyPath(p)
          oo.setDataSize(bytes.length.toLong)
          oo
      }
    }

    override def setOperationResult(operationResultOk: Boolean): Unit = ()
    override def setCompleted(complete: Long): Unit = ()
    override def setTotal(total: Long): Unit = ()
  }
}
