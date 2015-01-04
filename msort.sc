package week5

object msorts {
  def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case x :: xt => ys match {
      case Nil => xs
      case y :: yt =>
        if (x <= y) x::merge(xt, ys)
        else y::merge(xs, yt)
    }
  }                                               //> merge: (xs: List[Int], ys: List[Int])List[Int]
  
  def msort(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
      val n = xs.length / 2
      var (l, r) = xs splitAt n
      merge(msort(l), msort(r))
    }
  }                                               //> msort: (xs: List[Int])List[Int]
  
  msort(List(5,2,7,3,9,6))                        //> res0: List[Int] = List(2, 3, 5, 6, 7, 9)
}
