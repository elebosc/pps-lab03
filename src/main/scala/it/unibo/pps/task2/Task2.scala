package it.unibo.pps.task2

import it.unibo.pps.task1.Sequences.*
import it.unibo.pps.task1.Sequences.Sequence.{filter, flatMap, map}
import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{course, isTeacher}

object Task2:
  
  def getTeachersCourses(s: Sequence[Person]): Sequence[String] =
    map(filter(s)(p => isTeacher(p)))(p => course(p))
