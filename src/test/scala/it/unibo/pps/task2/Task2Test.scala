package it.unibo.pps.task2

import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{Student, Teacher}
import it.unibo.pps.task2.Task2.getTeachersCourses
import org.junit.Test
import org.junit.Assert.assertEquals

class Task2Test:

  import it.unibo.pps.task1.Sequences.*
  import Sequence.*
  
  @Test def testFilterOnEmptyListReturnsEmptyList(): Unit =
    val sequence: Sequence[Person] = Nil()
    val expectedSequence: Sequence[Person] = Nil()
    assertEquals(expectedSequence, getTeachersCourses(sequence))

  @Test def testFunctionReturnsCoursesCorrectly(): Unit =
    val course1 = "PPS"
    val course2 = "PCD"
    val student1 = Student("Nome1", 2000)
    val student2 = Student("Nome2", 2001)
    val teacher1 = Teacher("Nome3", course1)
    val teacher2 = Teacher("Nome4", course2)
    val sequence = Cons(student1, Cons(teacher1, Cons(teacher2, Cons(student2, Nil()))))
    val expectedSequence = Cons(course1, Cons(course2, Nil()))
    assertEquals(expectedSequence, getTeachersCourses(sequence))
    
    
