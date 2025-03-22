package u03

import scala.annotation.tailrec

object Task1:
  import Sequences.*
  import Sequence.*
  import u03.extensionmethods.Optionals.*
  import Optional.*

  /*
   * Skip the first n elements of the sequence
   * E.g., [10, 20, 30], 2 => [30]
   * E.g., [10, 20, 30], 3 => []
   * E.g., [10, 20, 30], 0 => [10, 20, 30]
   * E.g., [], 2 => []
   */
  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case _ => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
    case _ => Nil()

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */
  def min(s: Sequence[Int]): Optional[Int] =
    @tailrec
    def _min(s: Sequence[Int], min: Optional[Int]): Optional[Int] = (s, min) match
      case (Cons(h, t), Just(v)) if h < v => _min(t, Just(h))
      case (Cons(h, t), None()) => _min(t, Just(h))
      case _ => min
    _min(s, None())

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h1, t1) => t1 match {case Cons(h2, t2) => Cons(h1, evenIndices(t2)); case _ => Cons(h1, Nil())}
    case _ => Nil()

  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) if h == elem => true
    case Cons(h, t) => contains(t)(elem)
    case _ => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] =
    def _distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if contains(t)(h) => reverse(distinct(t))
      case Cons(h, t) => reverse(Cons(h, distinct(t)))
      case _ => Nil()
    _distinct(reverse(s))

end Task1


object Task2:
  import Sequences.*
  import Sequence.*
  import Task1.*
  import u02.Modules.*
  import Person.*

  def getTeacherCourses(s: Sequence[Person]): Sequence[String] =
    flatMap(s)(_ match {case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()})

  def foldLeft[A, B](s: Sequence[A])(default: B)(accFun: (B, A) => B): B =
    @tailrec
    def foldL[A, B](s: Sequence[A])(accFun: (B, A) => B)(acc: B): B = s match
      case Cons(h, t) => foldL(t)(accFun)(accFun(acc, h))
      case _ => acc
    foldL(s)(accFun)(default)

  def getNumberOfCourses(s: Sequence[Person]): Int =
    foldLeft(map(filter(s)(_ match {case Teacher(_, _) => true; case _ => false}))(_ => 1))(0)(_ + _)

end Task2


object Task3:
  import Streams.*
  import Stream.*

  def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
    case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
    case _ => empty()

  def fill[A](n: Int)(k: A): Stream[A] = n match
    case n if n > 0 => cons(k, fill(n - 1)(k))
    case _ => empty()

  def fibonacci(): Stream[Int] =
    def fib(f1: Int, f2: Int): Stream[Int] =
      cons(f1 + f2, fib(f2, f1 + f2))
    cons(0, cons(1, fib(0, 1)))

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
    case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1(), t2())))
    case (Empty(), _) => s2
    case _ => s1

  import Sequences.*
  import Sequence.*

  def cycle[A](lst: Sequence[A]): Stream[A] =
    def _cycle[A](lst: Sequence[A], tail: Sequence[A]): Stream[A] = tail match
      case Sequence.Cons(h, t) => cons(h, _cycle(lst, t))
      case _ => _cycle(lst, lst)
    _cycle(lst, lst)

end Task3



