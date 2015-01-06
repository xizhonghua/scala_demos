package week6

object polys {
  class Poly(_terms: Map[Int, Double]) {
    val terms = _terms withDefaultValue 0.0
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(ot: (Int, Double)): (Int, Double) = {
      val (exp, coff) = ot
      (exp, coff + terms(exp))
    }
    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield
        if (exp == 0) coeff
        else if(exp == 1) coeff + "*x"
        else coeff + "*x^" + exp
      ) mkString " + "
    }
    
  }
  
  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p1  : week6.polys.Poly = 6.2*x^5 + 4.0*x^3 + 2.0*x
  var p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))      //> p2  : week6.polys.Poly = 7.0*x^3 + 3.0
  
  p1 + p2                                         //> res0: week6.polys.Poly = 6.2*x^5 + 11.0*x^3 + 2.0*x + 3.0
  p1.terms(7)                                     //> res1: Double = 0.0
}
