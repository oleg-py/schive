package com.olegpy.schive

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ArchiveSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {
  behavior of "Archive#entries"

  it should "list elements of existing archive" in ???
  it should "return empty sequence if archive doesn't exist" in ???


  behavior of "Archive#extractSome"

  it should "extract provided contents" in ???
  it should "ignore excluded contents" in ???


  behavior of "Archive#spliceSome"

  it should "create new archive with provided data" in ???
  it should "remove elements mapped to None" in ???
  it should "replace elements mapped to Some" in ???
  it should "retain elements not mapped to anything" in ???
}
