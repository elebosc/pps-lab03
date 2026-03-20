package it.unibo.pps.task2

import it.unibo.pps.task1.Sequences.*
import Sequence.*
import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{course, isTeacher}

import scala.annotation.tailrec

object Task2:
  
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
