package com.phatlabs.ninja.chap6

class Rational(n: Int, d: Int) {
  require(d != 0)
  override def toString: String = n + "/" + d


  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1) // auxiliary constructor

  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def lessThan(that: Rational): Boolean =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Unit =
    if( this.lessThan(that)) that else this


  //Overloads plus operator for use with rationals
  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  //Overloads multiplication operator for use with raitonals
  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)


  private def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a % b)

}

object Scratch extends App {

  val oneHalf = new Rational(1,2)
//  val illegal = new Rational(5,0)
  val twoThirds = new Rational(2,3)

  println(oneHalf add twoThirds)

  println(oneHalf + twoThirds)

  println(oneHalf * twoThirds)

  println(oneHalf * oneHalf + twoThirds)


  //Produces illegal arg exception since denom is 0
//  println(illegal.toString)

}