/**
 * File   : naturals.scala
 * License: License: GNU v3.0
 * Author : Andrei Leonard Nicusan <a.l.nicusan@bham.ac.uk>
 * Date   : 01.09.2019
 */


package naturals

// Peano numbers

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}


object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor = throw new java.util.NoSuchElementException("Zero has no natural predecessor!")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = {
    if (that.isZero)
      Zero
    else
      throw new java.util.NoSuchElementException("A positive number cannot be subtracted from zero!")
  }
}


class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}








