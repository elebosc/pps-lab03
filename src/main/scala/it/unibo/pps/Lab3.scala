package it.unibo.pps

import it.unibo.pps.task1.Sequences.*
import Sequence.*
import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{course, isTeacher}
import it.unibo.pps.task3.Streams.Stream
import it.unibo.pps.task3.Streams.Stream.{Cons as StCons, Empty as StEmpty, cons}
import u03.Optionals.Optional
import u03.Optionals.Optional.{Empty, Just}

import scala.annotation.tailrec

object Lab3:

  /*
   * Task 1
   */

  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
    case (Cons(_, tail), n) if n > 0 => skip(tail)(n - 1)
    case (_, 0) => s
    case _ => Nil()

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (_, Nil()) => Nil()
    case (Nil(), _) => Nil()
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
    case (s1, Nil()) => s1
    case (Nil(), s2) => s2
    case (Cons(h1, t1), s2) => Cons(h1, concat(t1, s2))

  def reverse[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _reverse[B](s: Sequence[B])(acc: Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, Nil()) => Cons(h, acc)
      case Cons(h, t) => _reverse(t)(Cons(h, acc))
    _reverse(s)(Nil())

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = (s, mapper) match
    case (Nil(), m) => Nil()
    case (Cons(h, t), m) => concat(m(h), flatMap(t)(m))

  def min(s: Sequence[Int]): Optional[Int] =
    @tailrec
    def _min(s: Sequence[Int])(currentMin: Optional[Int]): Optional[Int] = (s, currentMin) match
      case (Nil(), currMin) => currMin
      case (Cons(h, t), Empty()) => _min(t)(Just(h))
      case (Cons(h, t), Just(currMin)) if h < currMin => _min(t)(Just(h))
      case (Cons(_, t), currMin) => _min(t)(currMin)
    _min(s)(Empty())

  def evenIndices[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _evenIndices[B](s: Sequence[B])(acc: Sequence[B])(currIdx: Int): Sequence[B] = (s, acc, currIdx) match
      case (Nil(), _, _) => acc
      case (Cons(h, t), acc, idx) if idx % 2 == 0 => _evenIndices(t)(Cons(h, acc))(idx + 1)
      case (Cons(_, t), acc, idx) => _evenIndices(t)(acc)(idx + 1)
    _evenIndices(reverse(s))(Nil())(0)

  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Nil() => false
    case Cons(h, _) if h.equals(elem) => true
    case Cons(_, t) => contains(t)(elem)

  def distinct[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def _distinct[B](s: Sequence[B])(acc: Sequence[B]): Sequence[B] = s match
      case Nil() => acc
      case Cons(h, t) if contains(acc)(h) => _distinct(t)(acc)
      case Cons(h, t) => _distinct(t)(Cons(h, acc))
    reverse(_distinct(s)(Nil()))

  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    @tailrec
    def _group[B](s: Sequence[B])(acc: Sequence[Sequence[B]])(prevElem: Optional[B]): Sequence[Sequence[B]] =
      (s, acc, prevElem) match
        case (Nil(), _, _) => acc
        case (Cons(h, t), _, Empty()) => _group(t)(Cons(Cons(h, Nil()), Nil()))(Just(h))
        case (Cons(h, t), Cons(hAcc, tAcc), Just(elem)) if h.equals(elem) =>
          _group(t)(Cons(Cons(h, hAcc), tAcc))(Just(h))
        case (Cons(h, t), acc, Just(_)) =>
          _group(t)(concat(acc, Cons(Cons(h, Nil()), Nil())))(Just(h))
    _group(s)(Nil())(Empty())

  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    @tailrec
    def _partition[B](s: Sequence[B])(pred: B => Boolean)(acc: (Sequence[B], Sequence[B])): (Sequence[B], Sequence[B]) =
      (s, acc) match
        case (Nil(), _) => acc
        case (Cons(h, t), (s1, s2)) if pred(h) => _partition(t)(pred)(concat(s1, Cons(h, Nil())), s2)
        case (Cons(h, t), (s1, s2)) => _partition(t)(pred)(s1, concat(s2, Cons(h, Nil())))
    _partition(s)(pred)((Nil(), Nil()))

  /*
   * Task 2
   */

  def getTeachersCourses(s: Sequence[Person]): Sequence[String] =
    map(filter(s)(p => isTeacher(p)))(p => course(p))

  def getDistinctCoursesNumber(s: Sequence[Person]): Int =
    @tailrec
    def _countItems(s: Sequence[String])(acc: Int): Int = s match
      case Nil() => acc
      case Cons(_, t) => _countItems(t)(acc + 1)
    _countItems(distinct(getTeachersCourses(s)))(0)

  def foldLeft(s: Sequence[Int])(k: Int)(f: (Int, Int) => Int): Int =
    def _foldLeft(s: Sequence[Int]): Int = s match
      case Nil() => k
      case Cons(h, Nil()) => f(k, h)
      case Cons(h, t) => f(_foldLeft(t), h)
    _foldLeft(reverse(s))

  /*
   * Task 3
   */

  def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
    case StEmpty() => StEmpty()
    case StCons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
    case _ => StEmpty()

  def fill[A](n: Int)(k: A): Stream[A] = n match
    case 0 => StEmpty()
    case x if x > 0 => cons(k, fill(n - 1)(k))

  val fibonacci: Stream[Int] =
    def _fibonacci(n: Int): Int = n match
      case 0 => 0
      case 1 => 1
      case x if x > 0 => _fibonacci(x - 1) + _fibonacci(x - 2)
    Stream.map(Stream.iterate(0)(_ + 1))(_fibonacci)
