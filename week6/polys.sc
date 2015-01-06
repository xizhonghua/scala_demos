package week6

object polys {
  class Poly(_terms: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = _terms withDefaultValue 0.0
    def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerms))
    def addTerms(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
      terms + (term._1 -> (term._2 + terms(term._1)))
    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield if (exp == 0) coeff
      else if (exp == 1) coeff + "*x"
      else coeff + "*x^" + exp) mkString " + "
    }

  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //> p1  : week6.polys.Poly = 6.2*x^5 + 4.0*x^3 + 2.0*x
  var p2 = new Poly(0 -> 3.0, 3 -> 7.0)           //> p2  : week6.polys.Poly = 7.0*x^3 + 3.0

  p1 + p2                                         //> res0: week6.polys.Poly = 6.2*x^5 + 11.0*x^3 + 2.0*x + 3.0
  p1.terms(7)                                     //> res1: Double = 0.0
  p2 + p2                                         //> res2: week6.polys.Poly = 14.0*x^3 + 6.0
}
