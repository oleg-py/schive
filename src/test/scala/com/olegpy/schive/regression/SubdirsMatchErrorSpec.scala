package com.olegpy.schive.regression

import java.nio.file.Paths

import better.files.File
import com.olegpy.schive.{Archive, UnitSpec}

class SubdirsMatchErrorSpec extends UnitSpec {
  val path = {
    val sub = """regression/MTS_SimFused_1424487_FancifulForestPottedTrees-BySimFused.zip"""
    val url = getClass.getClassLoader.getResource(sub)
    File(url.toURI).path
  }

  "Archive#mapSome" should "not fail with provided zip file" in {
    val arch = Archive(path)

    noException shouldBe thrownBy {
      val mapped = arch.mapSome(e => Some((bytes: Array[Byte]) => e))
      mapped should contain theSameElementsAs arch.files
    }
  }
}
