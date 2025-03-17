package u03

import org.junit.Assert.assertEquals
import org.junit.Test

class Task2Test:

  import u02.Modules.*
  import Person.*
  import Sequences.*
  import Sequence.*
  import Task2.*

  val sequence: Sequence[Person] = Cons(Student("mario", 2020), Cons(Teacher("luigi", "Programming"),
    Cons(Student("maria", 2021), Cons(Teacher("laura", "Statistics"), Nil()))))

  @Test def testGetTeacherCourses(): Unit =
    assertEquals(Cons("Programming", Cons("Statistics", Nil())), getTeacherCourses(sequence))


