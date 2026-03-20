package it.unibo.pps.task2

import it.unibo.pps.task1.Sequences.*
import Sequence.*
import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{Student, Teacher}
import it.unibo.pps.task2.Task2.{foldLeft, getDistinctCoursesNumber, getTeachersCourses}
import org.junit.Test
import org.junit.Assert.assertEquals

class Task2Test:
  
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

  @Test def testDistinctCoursesAreCountedCorrectlyOnNonEmptySequence(): Unit =
    val teacher1 = Teacher("Viroli", "PPS")
    val teacher2 = Teacher("Aguzzi", "PPS")
    val teacher3 = Teacher("Ricci", "PCD")
    val sequence = Cons(teacher1, Cons(teacher2, Cons(teacher3, Nil())))
    val expectedCoursesNumber = 2
    assertEquals(expectedCoursesNumber, getDistinctCoursesNumber(sequence))

  @Test def testDistinctCoursesReturnsZeroOnEmptySequence(): Unit =
    val sequence: Sequence[Person] = Nil()
    val expectedCoursesNumber = 0
    assertEquals(expectedCoursesNumber, getDistinctCoursesNumber(sequence))
    
  @Test def testFoldLeftReturnsCorrectResultOnNonEmptySequenceAndNullDefaultValue(): Unit =
    val sequence = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val defaultValue = 0
    val function: (Int, Int) => Int = _ - _
    val result = foldLeft(sequence)(defaultValue)(function)
    val expectedResult = -16
    assertEquals(expectedResult, result)

  @Test def testFoldLeftReturnsCorrectResultOnNonEmptySequenceAndPositiveDefaultValue(): Unit =
    val sequence = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val defaultValue = 3
    val function: (Int, Int) => Int = _ - _
    val result = foldLeft(sequence)(defaultValue)(function)
    val expectedResult = -13
    assertEquals(expectedResult, result)

  @Test def testFoldLeftReturnsCorrectResultOnNonEmptySequenceAndNegativeDefaultValue(): Unit =
    val sequence = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val defaultValue = -3
    val function: (Int, Int) => Int = _ - _
    val result = foldLeft(sequence)(defaultValue)(function)
    val expectedResult = -19
    assertEquals(expectedResult, result)

  @Test def testFoldLeftReturnsCorrectResultOnNonEmptySequenceWithNegativeValues(): Unit =
    val sequence = Cons(3, Cons(-7, Cons(1, Cons(-5, Nil()))))
    val defaultValue = 0
    val function: (Int, Int) => Int = _ - _
    val result = foldLeft(sequence)(defaultValue)(function)
    val expectedResult = 8
    assertEquals(expectedResult, result)

  @Test def testFoldLeftReturnsDefaultValueOnEmptySequence(): Unit =
    val sequence: Sequence[Int] = Nil()
    val defaultValue = 3
    val function: (Int, Int) => Int = _ - _
    val result = foldLeft(sequence)(defaultValue)(function)
    val expectedResult = defaultValue
    assertEquals(expectedResult, result)

