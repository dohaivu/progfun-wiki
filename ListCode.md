---
layout: page
title: List code in tutorial
---

## List code in tutorial

```scala
object list {
  var fruit: List[String] = List("apple", "orange", "mango", "lime")
  val f2 = List("1", "3")
      
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  //concate 2 lists using pattern matching
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: (zs ::: ys)
  }
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }
  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => xs
    case y :: ys => if(n >= 0 && xs(n) == y) removeAt(ys,n-1) else List(y) ::: removeAt(ys,n-1)
  }
  def removeAt2[T](n:Int,xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
  def flatten(xs: List[Any]):List[Any] = xs match {
    case Nil => Nil
    case Nil :: xs => flatten(xs)
    case (z :: zs) :: xs => z :: flatten(zs :: xs)
    case x :: zs => x :: flatten(zs)
  }
  
  def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case x :: xs1 => ys match {
      case Nil => xs
      case y :: ys1 => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }
  }
  
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil     => xs
    case y :: ys => y*y :: squareList(ys)
  }

  def squareListMap(xs: List[Int]): List[Int] =
    xs map ((x:Int) => x * x)
  
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => List((xs takeWhile (y => y == x))) ::: pack(xs dropWhile (y => y == x))
  }
  def pack2[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }
  pack2(List("a", "a", "a", "b", "c", "c", "a"))
  
  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil      => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      List((x, first.length)) ::: encode(rest)
  }
  encode(List("a", "a", "a", "b", "c", "c", "a"))
  
  merge(List(1, 5), List(2, 6))
  mergesort.msort(List(3, 5, 1, 2))((x:Int, y:Int) => x < y )
 
  def isPrime(x: Int): Boolean = (2 until x) forall (d => x % d != 0)
  isPrime(2013)
  
  val n = 7
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i,j))) filter (pair => isPrime(pair._1 + pair._2))
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
  } yield (i, j)

  //product of two list using for expression
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
    (for ((x, y) <- xs zip ys) yield (x*y)).sum
  scalarProduct(List(1,3,5), List(6,2, 4))


  val p1 = new polynomials.Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new polynomials.Poly(Map(0 -> 3.0, 3 -> 7.0))
}

//sort a list using pattern matching
object mergesort {
  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
      
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }
}

//using Map to express a quotation ex: x^3 + 2x^2 + 4 = 0
object polynomials {
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }
}
```



