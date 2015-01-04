package week5
import math.Ordering

object msorts {
 
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    if (xs.length <= 1) xs
    else {
		  def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
		    case (Nil, ys) => ys
		    case (xs, Nil) => xs
		    case (x::xt, y::yt) =>
		      if (ord.lt(x, y)) x::merge(xt, ys)
		      else y::merge(xs, yt)
		  }
      val n = xs.length / 2
      var (l, r) = xs splitAt n
      merge(msort(l), msort(r))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
  
  var nums = List(5,2,7,3,9,6)                    //> nums  : List[Int] = List(5, 2, 7, 3, 9, 6)
  var fruits = List("apple", "pineapple", "orange", "banana")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana)
  
  msort(nums)                                     //> res0: List[Int] = List(2, 3, 5, 6, 7, 9)
  msort(fruits)                                   //> res1: List[String] = List(apple, banana, orange, pineapple)
}
