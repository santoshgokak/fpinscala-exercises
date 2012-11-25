package fpinscala.structuringprograms

import math._

case class Box(height: Double, width: Double)

object Exercises {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  def wider(x: Box, y: Box): Box =
  greaterBy(x, y, _.width)

  def taller(x: Box, y: Box) =
  greaterBy(x, y, _.height)

  type Pred[A] = A => Boolean

  def absolute(f: Int => Int): Int => Int = n => abs(f(n))

  def divisibleBy(k: Int): Pred[Int] = n => n % k == 0

  //EXERCISE 5: Rewrite even so that it calls divisibleBy.
  def isEven = divisibleBy(2)

  /*EXERCISE 6: Write a predicate that checks whether a number is divisible by
  both 3 and 5. Write another to check whether a number is divisible by either 3 or 5.
  These will be almost identical. Eliminate as much of this duplication as you can by
  implementing a higher-order function, lift, that abstracts over the difference.
  Then define both predicates in terms of that function. Note that logical "or" in
  Scala is a function on Boolean called || and logical "and" is &&.*/
  def isDivisibleBy3 = divisibleBy(3)
  def isDivisibleBy5 = divisibleBy(5)
  def isDivisibleBy3and5 = {n:Int => isDivisibleBy3(n) && isDivisibleBy5(n)}
  def isDivisibleBy3or5 = {n:Int => isDivisibleBy3(n) || isDivisibleBy5(n)}

  def lift[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] = k => f(g(k),h(k))

  def isDivisibleBy3and5UsingLift = lift(((a,b) => a && b), isDivisibleBy3,isDivisibleBy5)
  def isDivisibleBy3or5UsingLift = lift(((a,b) => a || b), isDivisibleBy3,isDivisibleBy5)

  def isDivisibleBy3and5UsingLiftLessVerbose = lift(_ && _, isDivisibleBy3,isDivisibleBy5)
  def isDivisibleBy3or5UsingLiftLessVerbose = lift(_ || _, isDivisibleBy3,isDivisibleBy5)


  def curry[A,B,C](f: (A, B) => C): A => B => C = a => f(a,_)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = f(_)(_)

  def compose[A,B,C](f: B => C, g: A => B): A => C =  x => f(g(x)) //can also use "f compose g"

  //lift as polymorphic as possible
  def liftPoly[A,B,C,D](f: (B, C) => D)(g: A => B, h: A => C): A => D = {a => f(g(a),h(a)) }

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E =  {a => f(g(a),h(a),i(a)) }

  //EXERCISE 11 (optional, hard): Reuse the lift function in your definition of lift3.
  def lift3Ex11[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E =  sys.error("todo") //a => liftPoly(f )(g,h)  

  def fib(n: Int): Int = {
    def fib0(a:Int,b : Int,counter :Int = 0):Int = if (counter == n) a else fib0(b,a+b,counter + 1)

    fib0(0,1) 
  }

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // We want to find the `x` such that `x` squared minus `n` equals `0`.
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // Starting with a guess of `2.0`, iteratively improve the guess.
                      x => f(x).abs > 1e-14) // `1e-14` is a way of writing `10` to the `-14`th power, a rather small number. When the difference between the guess and the answer is smaller than this, the guess is "good enough".
  }
  
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =  {

    def loop(refined:A):A = if(!p(refined)) refined else loop(f(refined))

    loop(a)

  }
}