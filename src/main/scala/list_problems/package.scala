import scala.annotation.tailrec

package object list_problems {

  def encode[A](elements: List[A]): List[(Int, A)] = {
    pack(elements) map { run =>
      (run.length, run.head)
    }
  }

  sealed trait Encoded[V] {
    def decodeTo(accumulator: List[V]): List[V]
  }

  case class Run[V](length: Int, value: V) extends Encoded[V] {
    def decodeTo(accumulator: List[V]): List[V] = {
      List.fill(length)(value) ++ accumulator
    }
  }

  case class One[V](value: V) extends Encoded[V] {
    def decodeTo(accumulator: List[V]): List[V] = {
      value :: accumulator
    }
  }

  def encodeModified[A](elements: List[A]): List[Encoded[A]] = encode(elements) map {
    case (1, e) => One(e)
    case (length, e) => Run(length, e)
  }

  def decodeModified[V](coded: List[Encoded[V]]): List[V] = {
    coded.foldLeft(List[V]()) { (accumulator, element) =>
      element decodeTo accumulator
    }.reverse
  }

  @tailrec
  def last[A](elements: List[A]): A = {
    elements match {
      case Nil => throw new NoSuchElementException
      case List(e) => e
      case _ :: rest => last(rest)
    }
  }

  @tailrec
  def penultimate[A](elements: List[A]): A = {
    elements match {
      case List(e, _) => e
      case _ :: rest => penultimate(rest)
      case _ => throw new NoSuchElementException
    }
  }

  @tailrec
  def nth[A](index: Int, elements: List[A]): A = {
    (elements, index) match {
      case (e :: _, 0) => e
      case (_ :: rest, i) if i > 0 => nth(i - 1, rest)
      case _ => throw new NoSuchElementException
    }
  }

  def length[A](elements: List[A]): Int = length(elements, 0)

  @tailrec
  def length[A](elements: List[A], offset: Int): Int = {
    elements match {
      case Nil => offset
      case _ :: rest => length(rest, offset + 1)
    }
  }

  @tailrec
  def reverseInto[A](elements: List[A], suffix: List[A]): List[A] = {
    elements match {
      case Nil => suffix
      case x :: rest => reverseInto(rest, x :: suffix)
    }
  }

  def reverse[A](elements: List[A]): List[A] = reverseInto(elements, List())

  def isPalindrome[A](elements: List[A]): Boolean = isPalindrome(elements, List())

  @tailrec
  def isPalindrome[A](elements: List[A], suffix: List[A]): Boolean = {
    elements match {
      case _ :: `suffix` => true
      case `suffix` => true
      case x :: rest => isPalindrome(rest, x :: suffix)
      case Nil => false
    }
  }

  def flatten(elements: List[Any]): List[Any] = {
    elements match {
      case List() => List()
      case (l: List[Any]) :: rest => flatten(l) ++ flatten(rest)
      case x :: rest => x :: flatten(rest)
    }
  }

  def compress[A](elements: List[A]): List[A] = reverseCompressInto(elements, List()).reverse

  def reverseCompressInto[A](elements: List[A], target: List[A]): List[A] = {
    elements match {
      case Nil => target
      case l :: ltail => target match {
        case `l` :: rest => reverseCompressInto(ltail, target)
        case _ => reverseCompressInto(ltail, l :: target)
      }
    }
  }

  def pack[A](elements: List[A]): List[List[A]] = {
    val (run, rest) = splitInitialRun(elements, List())
    rest match {
      case Nil => List(run)
      case _ => run :: pack(rest)
    }
  }

  /*
    Removes the first stretch of elements that are identical to those in currentStretch
    and prepends them to currentStretch. Returns the thus resulting stretch and the resulting remainder of the elements.
   */
  @tailrec
  def splitInitialRun[A](elements: List[A], currentRun: List[A]): (List[A], List[A]) = {
    elements match {
      case Nil => (currentRun, Nil)
      case head :: tail => currentRun match {
        case Nil => splitInitialRun(tail, List(head))
        case `head` :: _ => splitInitialRun(tail, head :: currentRun)
        case _ => (currentRun, elements)
      }
    }
  }

  /*
    Helper function that replicates each of the elements of a given list n times into the given output list (in reverse
    order, since we decompose the input list recursively)
  */

  @tailrec
  def replicateElementsInto[V](elements: List[V], currentCount: Int, desiredCount: Int, result: List[V]): List[V] = {
    elements match {
      case Nil => result
      case head :: tail => currentCount match {
        case 0 => replicateElementsInto(tail, desiredCount, desiredCount, result)
        case n => replicateElementsInto(elements, n - 1, desiredCount, head :: result)
      }
    }
  }

  def duplicateElements[V](elements: List[V]): List[V] = replicateElements(2, elements)

  def replicateElements[V](count: Int, elements: List[V]) = replicateElementsInto(elements, count, count, List()).reverse

  def dropEveryNth[V](count: Int, elements: List[V]): List[V] = {
    require(count >= 1, "Count must be greater than or equal to 1.")
    dropEveryNthElementInto(elements, count, count, List()).reverse
  }

  @tailrec
  def dropEveryNthElementInto[V](elements: List[V], currentCount: Int, desiredCount: Int, result: List[V]): List[V] = {
    //It's amazing how similar this is to the recursion in replicateElementsInto
    elements match {
      case Nil => result
      case head :: tail => currentCount match {
        case 1 => dropEveryNthElementInto(tail, desiredCount, desiredCount, result)
        case n => dropEveryNthElementInto(tail, n - 1, desiredCount, head :: result)
      }
    }
  }
}
