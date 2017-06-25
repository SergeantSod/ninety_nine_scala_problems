package list_problems

import org.scalatest.{FunSpec, Matchers}
import list_problems._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.{BeMatcher, MatchResult, Matcher}

import scala.reflect.ClassTag

class ListProblemSpecs extends FunSpec with Matchers with TypeCheckedTripleEquals {

  it("finds the last element"){
    last(List(1, 1, 2, 3, 5, 8)) should ===(8)
  }

  it("finds the last but second to last element"){
    penultimate(List(1, 1, 2, 3, 5, 8)) should ===(5)
  }

  it("finds an element at a given index"){
    nth(2, List(1, 1, 2, 3, 5, 8)) should ===(2)
  }

  it("finds the number of elements in a list"){
    list_problems.length(List(1, 1, 2, 3, 5, 8)) should ===(6)
  }

  it("reverses a list"){
    reverse(List(1, 1, 2, 3, 5, 8)) should ===(List(8, 5, 3, 2, 1, 1))
  }

  it("detects palindromes"){
    isPalindrome(List(1, 2, 3, 2, 1)) should ===(true)
    isPalindrome(List(1, 2, 2, 1)) should ===(true)

    isPalindrome(List(1, 2, 3, 6, 1)) should ===(false)
    isPalindrome(List(1, 2, 2, 6)) should ===(false)
  }

  it("flattens a nested list structure"){
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should ===(List(1, 1, 2, 3, 5, 8))
    flatten(List()) should ===(List())
  }

  it("eliminates subsequent duplicates in a list"){
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should ===(List('a, 'b, 'c, 'a, 'd, 'e))
    compress(List()) should ===(List())

    compress(List('a, 'b, 'c, 'a, 'd, 'e)) should ===(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it("packs subsequent duplicates into sublists"){
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should ===(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  it("creates a run-length-endcoding"){
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should ===(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  it("creates a modified run-length encoding"){
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should ===(List(Run(4,'a), One('b), Run(2,'c), Run(2,'a), One('d), Run(4,'e)))
  }

  it("decodes a run-length encoded list"){
    val someList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    decodeModified(encodeModified(someList)) should ===(someList)
  }

  it("performs a run-length encoding directly")(pending)

  it("duplicates the elements of a list"){
    duplicateElements(List('a, 'b, 'c, 'c, 'd)) should ===(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  it("duplicates the elements of a list a given number of times"){
    replicateElements(3, List('a, 'b, 'c, 'c, 'd)) should ===(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  it("drops every n-th element from a list"){
    dropEveryNth(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should ===(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  it("splits a list into two parts"){
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should ===(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  it("extracts a slice from a list"){
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should ===(List('d, 'e, 'f, 'g))
  }
}
