package com.olegpy.schive.regression

import better.files.File
import com.olegpy.schive.{Archive, UnitSpec}

class InitErrorInParallel extends UnitSpec {
  val archives = for {
    (kind, ext) <- Seq("multi" -> "7z", "multi" -> "tar", "multi" -> "zip", "single" -> "txt.bz2", "single" -> "txt.gz")
    resourcePath = s"updatable/${kind}_element_archive.$ext"
    uri = getClass.getClassLoader.getResource(resourcePath).toURI
    file = File(uri)
  } yield file.path

  "Schive#mapSome" should "not raise errors related to SevenZipJBinding init in parallel" in {
    val coll = archives.par.map(Archive(_))
    noException shouldBe thrownBy {
      coll.map(a => a.mapSome(e => Some((_: Any) => e)))
    }
  }
}
