package week5

object high_order_func {
  def scaleList(xs: List[Double], factor: Double) =
    xs map (x => x * factor)                      //> scaleList: (xs: List[Double], factor: Double)List[Double]
  scaleList(List(1,2,3), 5)                       //> res0: List[Double] = List(5.0, 10.0, 15.0)
  
  def squareListP(xs: List[Double]): List[Double] = xs match {
    case Nil => xs
    case y::ys => y*y::squareListP(ys)
  }                                               //> squareListP: (xs: List[Double])List[Double]
  squareListP(List(1,2,3))                        //> res1: List[Double] = List(1.0, 4.0, 9.0)
  
  def squareList(xs: List[Double]) =
    xs map (x => x * x)                           //> squareList: (xs: List[Double])List[Double]
  squareList(List(1,2,3))                         //> res2: List[Double] = List(1.0, 4.0, 9.0)
  
  // retivel positve elements of a list
  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y::ys => if (y>0) y::posElems(ys) else posElems(ys)
  }                                               //> posElems: (xs: List[Int])List[Int]
  posElems(List(3,0,-1,5,9,-2))                   //> res3: List[Int] = List(3, 5, 9)
  
  def posElemsF(xs: List[Int]): List[Int] =
    xs filter (x => x>0)                          //> posElemsF: (xs: List[Int])List[Int]
  posElemsF(List(3,0,-1,5,9,-2))                  //> res4: List[Int] = List(3, 5, 9)
  
  val data = List('a', 'a', 'a', 'b', 'c', 'c', 'd')
                                                  //> data  : List[Char] = List(a, a, a, b, c, c, d)
  
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x::xt =>
      val (first, rest) = xs span(y=>y == x)
      first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  
  pack(data)                                      //> res5: List[List[Char]] = List(List(a, a, a), List(b), List(c, c), List(d))
  
  // run-length encoding
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))     //> encode: [T](xs: List[T])List[(T, Int)]
    
  encode(data)                                    //> res6: List[(Char, Int)] = List((a,3), (b,1), (c,2), (d,1))
}
