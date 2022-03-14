package u03

import u02.Optionals.Option

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    import Option.*

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case list => list

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def map2[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter2[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(v => if pred(v) then Cons(v, Nil()) else Nil())

    def max(l: List[Int]): Option[Int] =
      def maxIteration(l: List[Int], currentMax: Option[Int]): Option[Int] = (l, currentMax) match
        case (Cons(h, t), Some(x)) if (h <= x) => maxIteration(t, Some(x))
        case (Cons(h, t), _) => maxIteration(t, Some(h))
        case (Nil(), current) => current

      maxIteration(l, None())

    def foldLeft[A](l: List[A])(value: A)(operator: (A, A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(operator(value, h))(operator)
      case Nil() => value

    def foldRight[A](l: List[A])(value: A)(operator: (A, A) => A): A = l match
      case Cons(h, t) => operator(h, foldRight(t)(value)(operator))
      case Nil() => value


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
