package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import u03.Lists.*

class StreamTest:
  import Stream.*
  import List.*

  @Test def testDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Nil(), Stream.toList(Stream.drop(Stream.empty())(3)))

  @Test def testConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(constant("x"))(5)))
    assertEquals(Cons(1, Cons(1, Cons(1, Cons(1, Nil())))), Stream.toList(Stream.take(constant(1))(4)))

  @Test def testFibs() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13 ,Nil())))))))),
      Stream.toList(Stream.take(Stream.fibonacci())(8)))
