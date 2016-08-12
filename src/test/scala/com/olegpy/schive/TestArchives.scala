package com.olegpy.schive

import org.scalatest.prop.Tables

object TestArchives extends Tables {
  sealed trait Quantity
  case object Single extends Quantity
  case object Multi extends Quantity

  def readOnly = Table(("Archive path",                           "Element quantity")
                     , ("extractable/multi_element_archive.rar",  Multi)
  )

  def readWrite = Table(("Archive path",                             "Element quantity")
                      , ("updatable/multi_element_archive.7z",       Multi)
                      , ("updatable/multi_element_archive.tar",      Multi)
                      , ("updatable/multi_element_archive.zip",      Multi)
                      , ("updatable/single_element_archive.txt.bz2", Single)
                      , ("updatable/single_element_archive.txt.gz",  Single)
  )
}
