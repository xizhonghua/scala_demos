package week6

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      var qs = (row -1 to 0 by -1) zip queens
      qs forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }
    
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          qs <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, qs)
        } yield col :: qs
    }
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  
 
 (queens(4) take 3 map show) mkString "\n"        //> res0: String = "
                                                  //| * X * * 
                                                  //| * * * X 
                                                  //| X * * * 
                                                  //| * * X * 
                                                  //| 
                                                  //| * * X * 
                                                  //| X * * * 
                                                  //| * * * X 
                                                  //| * X * * "
  
}
