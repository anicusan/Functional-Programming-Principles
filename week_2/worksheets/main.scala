
object Prod {

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    @annotation.tailrec
    def productRec(acc: Int, a: Int): Int = {
      if (a > b) acc else productRec(acc * f(a), a + 1)
    }
    productRec(1, a)
  }

  def factorial(n: Int): Int = product(x => x)(1, n)

  def intervalFunc(f: Int => Int, unit: Int, comb: (Int, Int) => Int)(a: Int, b: Int): Int = {
    @annotation.tailrec
    def intervalFuncRec(acc: Int, a: Int): Int = {
      if (a > b) acc else intervalFuncRec(comb(f(a), acc), a + 1)
    }
    intervalFuncRec(unit, a)
  }

}


object Exercise {

  import math.abs

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double): Boolean = 
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)



}




