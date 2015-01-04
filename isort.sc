package week4

object isort {

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]
  
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x<=y) x::xs else y::insert(x, ys)
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]
  
  
  var list = List(2,9,3,7,5,4)                    //> list  : List[Int] = List(2, 9, 3, 7, 5, 4)
  isort(list)                                     //> res0: List[Int] = List(2, 3, 4, 5, 7, 9)
}
