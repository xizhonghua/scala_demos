object ws {
  def abs(x:Double) = if (x>0) x else -x          //> abs: (x: Double)Double
  
  def isGoodEnough(guess:Double, x:Double) =
    if (x == 0) abs(guess) < 1e-5 else
    if (abs(guess*guess - x) / x < 1e-5) true else false
                                                  //> isGoodEnough: (guess: Double, x: Double)Boolean
  
  def improve(guess:Double, x:Double) =
    (guess + x/guess)/2                           //> improve: (guess: Double, x: Double)Double
  
  def sqrtIter(guess:Double, x: Double) : Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess,x), x)            //> sqrtIter: (guess: Double, x: Double)Double
   
  def sqrt(x: Double): Double =
    sqrtIter(1.0, x)                              //> sqrt: (x: Double)Double
   
   sqrt(0)                                        //> res0: Double = 7.62939453125E-6
   sqrt(1)                                        //> res1: Double = 1.0
   sqrt(2)                                        //> res2: Double = 1.4142156862745097
   sqrt(4)                                        //> res3: Double = 2.0000000929222947
   sqrt(1e-6)                                     //> res4: Double = 0.0010000001533016628
   sqrt(1e60)                                     //> res5: Double = 1.0000000031080746E30
}
