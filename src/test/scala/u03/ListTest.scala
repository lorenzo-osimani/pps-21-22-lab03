package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.*

import u02.Modules.*

class ListTest:
  import List.*
  import Option.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def TestDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop (l , 1))
    assertEquals(Cons(30, Nil()), drop(l , 2))
    assertEquals(Nil(), drop (l , 5))

  @Test def TestAppend() =
    val tail = Cons (40 , Nil ())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l , tail ))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))),
      List.flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      List.flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  import Person.*

  @Test def testFromPeopleToCourses() =
    val people = Cons(Person.Teacher("Pippo", "Matematica"),
      Cons(Person.Student("Lorenzo", 2022), Cons(Person.Teacher("Topolino", "Fisica"), Nil())))
    assertEquals(Cons("Matematica", Cons("Fisica", Nil())), fromPeopleToCourses(people))
    assertEquals(Cons("Matematica", Cons("Fisica", Nil())), fromPeopleToCourses2(people))

  @Test def testFold() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(0, foldLeft(Nil())(0)(_ - _))