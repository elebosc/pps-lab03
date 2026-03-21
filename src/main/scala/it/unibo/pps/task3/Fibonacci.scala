package it.unibo.pps.task3

object Fibonacci:

  val fibonacci: Streams.Stream[Int] =
    def _fibonacci(n: Int): Int = n match
      case 0 => 0
      case 1 => 1
      case x if x > 0 => _fibonacci(x - 1) + _fibonacci(x - 2)
    Streams.Stream.map(Streams.Stream.iterate(0)(_ + 1))(_fibonacci)
