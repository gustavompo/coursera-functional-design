package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues( namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for (
      kv <- namedExpressions;
      a = ( kv._1, Signal{ eval( kv._2(), namedExpressions ) })
    ) yield a
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def internalEval(expr: Expr, evaluated: Set[Expr], references: Map[String, Signal[Expr]]): Double = {
      def handleCyclic(ex: Expr): Expr = if (evaluated.contains(ex)) Literal(Double.NaN) else ex

      def op( a:Expr, b:Expr, fn:(Double,Double) => Double ) = {
        fn( internalEval( handleCyclic(a), evaluated + a, references), internalEval( handleCyclic(b), evaluated + b, references) )
      }
      expr match {
        case Literal(v) => v
        case Plus(a, b) => op(a, b, _ + _ )
        case Minus(a, b) => op(a, b, _ - _ )
        case Times(a, b) => op(a, b, _ * _ )
        case Divide(a, b) => op(a, b, _ / _ )
        case Ref(nm) => {
          val res = handleCyclic( getReferenceExpr(nm, references) )
          internalEval(res, evaluated + res, references)
        }
      }
    }
    internalEval(expr, Set[Expr](), references)
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
