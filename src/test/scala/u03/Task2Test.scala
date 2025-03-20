package u03

import org.junit.Assert.assertEquals
import org.junit.Test

class Task2Test:

  import u02.Modules.*
  import Person.*
  import Sequences.*
  import Sequence.*
  import Task2.*

  val personSequence: Sequence[Person] = Cons(Student("mario", 2020), Cons(Teacher("luigi", "Programming"),
    Cons(Student("maria", 2021), Cons(Teacher("laura", "Statistics"), Nil()))))
  val intSequence: Sequence[Int] = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))

  @Test def testGetTeacherCourses(): Unit =
    assertEquals(Cons("Programming", Cons("Statistics", Nil())), getTeacherCourses(personSequence))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(intSequence)(0)(_ - _))
    
  @Test def testGetNumberOfCourses(): Unit =
    assertEquals(2, getNumberOfCourses(personSequence))

