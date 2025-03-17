package u03

object Task2 extends App:

  import u02.Modules.*
  import Person.*
  import Sequences.*
  import Sequence.*

  def getTeacherCourses(s: Sequence[Person]): Sequence[String] = //s match
    /*case Cons(h, t) => h match
      case Teacher(n, c) => concat(Cons(c, Nil()), getTeacherCourses(t))
      case _ => getTeacherCourses(t)
    case _ => Nil()*/
    //map(filter(s)(_ match {case Teacher(_, _) => true; case _ => false}))(_ match {case Teacher(n, c) => c})
    flatMap(s)(_ match {case Teacher(_, c) => Cons(c, Nil()); case _ => Nil()})

  def foldLeft[A, B](s: Sequence[A])(default: B)(accFun: (B, A) => B): B =
    def foldL[A, B](s: Sequence[A])(accFun: (B, A) => B)(acc: B): B = s match
      case Cons(h, t) => foldL(t)(accFun)(accFun(acc, h))
      case _ => acc
    foldL(s)(accFun)(default)