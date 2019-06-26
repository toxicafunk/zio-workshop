// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

import java.time.LocalDate

import scala.util.Try

/**
 * Functions are total, deterministic, and free of side effects.
 */
object functions {

  /**
   * EXERCISE 1
   *
   * Convert the following partial procedure into a function.
   */
  def parseInt1(s: String): Int   = s.toInt
  def parseInt2( s: String ): Try[Int] = Try(s.toInt)
  def parseInt3( s: String ): Option[Int] = Try(s.toInt).fold(_ => None, i => Some(i))

  //EXERCISE 2
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] = if (b == 0) Some(a/b) else None

  //EXERCISE 3
  def head1[A](l: Seq[A]): A     = l.head
  def head2[A](l: Seq[A]): Option[A] = l.headOption

  //EXERCISE 4
  def secondChar1(str: String): Char = str.charAt(2)
  def secondChar2(str: String): Option[Char] = if (str.size >= 3) Some(str.charAt(2)) else None

  //EXERCISE 5
  def abs(n: Int): Int = if (n < 0) throw new Exception(s"$n should be positive") else n
  def absF(n: Int): Option[Int] = if (n < 0) None else Some(n)

  /**
   * EXERCISE 3
   *
   * Convert the following non-deterministic procedure into a deterministic function.
   */
  def increment1: Int              = scala.util.Random.nextInt(0) + 1
  def increment2(i: Int): Int = i + 1

  /**
   * EXERCISE 4
   *
   * Convert the following non-deterministic procedure into a deterministic function.
   */
  def nextDay1: LocalDate        = LocalDate.now.plusDays(1)
  def nextDay2(d: LocalDate): LocalDate = d.plusDays(1)
  //EXERCISE 3
  case object IncorrectAge extends Exception
  def computeAge1(year: Int): Int = {
    val age = LocalDate.now.getYear - year
    if (age < 0) throw IncorrectAge
    else age
  }
  def computeAge2(year: Int): Option[Int] = {
    val age = LocalDate.now.getYear() - year
    if (age < 0) None else Some(age)
  }

  /**
   * EXERCISE 5
   *
   * Convert the following side-effecting procedure into a pure function.
   */
  def get1(a: Int): Int = {
    println(s"the value is: $a")
    a
  }
  def get2( /* ??? */ ): ??? = ???

  //EXERCISE 2
  def sumN1(n: Int): Int = {
    var result = 0
    (1 to n).foreach(i => result = result + i)
    result
  }
  def sumN2(n: Int): Int = (1 to n).sum
  def sumN3(n: Int): Int = (1 to n).fold(0)(_ + _)

  //EXERCISE 3
  def updateArray1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i)))
  def updateArray2[A]( /* ??? */ ): ??? = ???

  /**
   * EXERCISE 7
   *
   * Design a purely functional API for drawing a bitmap image on a canvas.
   */
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x      = 0
    var y      = 0

    def goLeft(): Unit  = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit    = y += 1
    def goDown(): Unit  = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }
  def draw2( /* ... */ ): ??? = ???

}
