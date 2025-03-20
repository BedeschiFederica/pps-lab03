package u03

import org.junit.*
import org.junit.Assert.*

//import u03.extensionmethods.Sequences.*

class Task1Test:
  import u03.extensionmethods.Optionals.*
  import Optional.*
  import u03.Sequences.*
  import Sequence.*
  import u03.Task1.*

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Sequence.map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), Sequence.map(sequence)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(None(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup() =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped =
      Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(group(sequence), grouped)
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition() =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)

end Task1Test


class Task2Test:
  import u03.Sequences.*
  import Sequence.*
  import u02.Modules.*
  import Person.*
  import u03.Task2.*

  val personSequence: Sequence[Person] = Cons(Student("mario", 2020), Cons(Teacher("luigi", "Programming"),
    Cons(Student("maria", 2021), Cons(Teacher("laura", "Statistics"), Nil()))))
  val intSequence: Sequence[Int] = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))

  @Test def testGetTeacherCourses(): Unit =
    assertEquals(Cons("Programming", Cons("Statistics", Nil())), getTeacherCourses(personSequence))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(intSequence)(0)(_ - _))

  @Test def testGetNumberOfCourses(): Unit =
    assertEquals(2, getNumberOfCourses(personSequence))

end Task2Test


class Task3Test:
  import u03.Streams.*
  import Stream.*
  import u03.Task3.*

  val stream: Stream[Int] = cons(0, cons(2, cons(3, cons(7, empty()))))
  val pred: Int => Boolean  = _ < 5
  val resultStream: Stream[Int] = cons(0, cons(2, cons(3, empty())))

  @Test def testTakeWhile(): Unit =
    assertEquals(toList(resultStream), toList(takeWhile(stream)(pred)))