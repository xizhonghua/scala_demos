package week6

object fors {
  def isPrime(n: Int):Boolean =
    (2 until n) forall (d => n%d!=0)              //> isPrime: (n: Int)Boolean
  isPrime(3)                                      //> res0: Boolean = true
  isPrime(19)                                     //> res1: Boolean = true
  
  val n = 7                                       //> n  : Int = 7
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
  } yield (i, j)                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
                                                  
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x,y) <- xs zip ys) yield x*y).sum      //> scalarProduct: (xs: List[Double], ys: List[Double])Double
    
  scalarProduct(List(1.0, 2.0, 3.0), List(3.0, 2.0, 1.0))
                                                  //> res3: Double = 10.0
}
