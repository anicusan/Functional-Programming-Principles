/**
 * File   : isort.scala
 * License: GNU v3.0
 * Author : Andrei Leonard Nicusan <a.l.nicusan@bham.ac.uk>
 * Date   : 11.09.2019
 */

package isort

object Test {

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => x :: Nil
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  def isort(x: List[Int]): List[Int] = x match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

}
