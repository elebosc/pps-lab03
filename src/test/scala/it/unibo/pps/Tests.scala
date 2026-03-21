package it.unibo.pps

import it.unibo.pps.task1.Sequences.Sequence
import it.unibo.pps.task1.Sequences.Sequence.{Cons, Nil, sum, map, filter}
import it.unibo.pps.u03.Optionals.Optional
import it.unibo.pps.u03.Optionals.Optional.{Just, Empty}
import it.unibo.pps.task2.People.Person
import it.unibo.pps.task2.People.Person.{Student, Teacher}
import it.unibo.pps.Lab3.*
import it.unibo.pps.task3.Streams
import org.junit.*
import org.junit.Assert.*

class Task1Test:

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(sequence)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip(): Unit =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse(): Unit =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices(): Unit =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains(): Unit =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup(): Unit =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped =
      Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(grouped, group(sequence))
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition(): Unit =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)


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


class Task3Test:

  @Test def testTakeWhileOnNonEmptyStream(): Unit =
    val initValue = 0
    val nextFn: Int => Int = _ + 1
    val pred: Int => Boolean = _ < 5
    val stream = Streams.Stream.iterate(initValue)(nextFn)
    val sequence = Streams.Stream.toList(Streams.Stream.takeWhile(stream)(pred))
    val expectedSequence = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expectedSequence, sequence)

  @Test def testTakeWhileOnEmptyStream(): Unit =
    val pred: Int => Boolean = _ < 5
    val stream = Streams.Stream.empty()
    val sequence = Streams.Stream.toList(Streams.Stream.takeWhile(stream)(pred))
    val expectedSequence = Nil()
    assertEquals(expectedSequence, sequence)

  @Test def testFillOnNonEmptyStream(): Unit =
    val k = "a"
    val n = 3
    val sequence = Streams.Stream.toList(Streams.Stream.fill(n)(k))
    val expectedSequence = Cons(k, Cons(k, Cons(k, Nil())))
    assertEquals(expectedSequence, sequence)

  @Test def testFillEmptyStream(): Unit =
    val k = "a"
    val n = 0
    val sequence = Streams.Stream.toList(Streams.Stream.fill(n)(k))
    val expectedSequence = Nil()
    assertEquals(expectedSequence, sequence)

  @Test def testFillWithNegativeItemsNumberReturnsEmptyStream(): Unit =
    val k = "a"
    val n = 0
    val sequence = Streams.Stream.toList(Streams.Stream.fill(n)(k))
    val expectedSequence = Nil()
    assertEquals(expectedSequence, sequence)

  @Test def testFibonacci(): Unit =
    val n = 7
    val sequence = Streams.Stream.toList(Streams.Stream.take(fibonacci)(n))
    val expectedSequence = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Nil())))))))
    assertEquals(expectedSequence, sequence)

  @Test def testFibonacciEmptyStream(): Unit =
    val n = 0
    val sequence = Streams.Stream.toList(Streams.Stream.take(fibonacci)(n))
    val expectedSequence = Nil()
    assertEquals(expectedSequence, sequence)

  @Test def testFibonacciWithNegativeItemsNumberReturnsEmptyStream(): Unit =
    val n = -1
    val sequence = Streams.Stream.toList(Streams.Stream.take(fibonacci)(n))
    val expectedSequence = Nil()
    assertEquals(expectedSequence, sequence)