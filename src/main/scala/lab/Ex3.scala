package lab

import u02.Modules.Person.Teacher
import u02.Modules.{Person, isStudent}
import u03.Lists.List
import u03.Lists.List.*

object Ex3 extends App :

  import List.*
  import Person.*

  def fromPeopleToCourses(l: List[Person]): List[String] =
    val teachers = filter[Person](l)(!isStudent(_))
    map(teachers)(p => p match
      case Teacher(_, course) => course
    )

  def fromPeopleToCourses2(l: List[Person]): List[String] =
    flatMap(l)(p => p match
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    )
