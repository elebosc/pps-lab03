package it.unibo.pps.task3

import it.unibo.pps.task3.Streams
import org.junit.Test
import org.junit.Assert.assertEquals
import it.unibo.pps.task1.Sequences.Sequence.{Cons, Nil}
import it.unibo.pps.task3.Fibonacci.fibonacci

class Task3Test:

  @Test def testTakeWhileOnNonEmptyStream(): Unit =
    val initValue = 0
    val nextFn: Int => Int = _ + 1
    val pred: Int => Boolean = _ < 5
    val stream = Streams.Stream.iterate(initValue)(nextFn)
    val result = Streams.Stream.toList(Streams.Stream.takeWhile(stream)(pred))
    val expectedResult = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expectedResult, result)

  @Test def testTakeWhileOnEmptyStream(): Unit =
    val pred: Int => Boolean = _ < 5
    val stream = Streams.Stream.empty()
    val result = Streams.Stream.toList(Streams.Stream.takeWhile(stream)(pred))
    val expectedResult = Nil()
    assertEquals(expectedResult, result)

  @Test def testFillOnNonEmptyStream(): Unit =
    val k = "a"
    val n = 3
    val result = Streams.Stream.toList(Streams.Stream.fill(n)(k))
    val expectedResult = Cons(k, Cons(k, Cons(k, Nil())))
    assertEquals(expectedResult, result)

  @Test def testFillEmptyStream(): Unit =
    val k = "a"
    val n = 0
    val result = Streams.Stream.toList(Streams.Stream.fill(n)(k))
    val expectedResult = Nil()
    assertEquals(expectedResult, result)

  @Test def testFibonacci(): Unit =
    val n = 7
    val result = Streams.Stream.toList(Streams.Stream.take(fibonacci)(n))
    val expectedResult = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Nil())))))))
    assertEquals(expectedResult, result)

  @Test def testFibonacciEmptyStream(): Unit =
    val n = 0
    val result = Streams.Stream.toList(Streams.Stream.take(fibonacci)(n))
    val expectedResult = Nil()
    assertEquals(expectedResult, result)
