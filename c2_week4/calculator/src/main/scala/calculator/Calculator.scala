package calculator


sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues { expr => Var{ eval(expr(), namedExpressions) } }
  }

  /**
    * We have to protect against the case:
    *
    *   'a' = Ref('b')
    *   'b' = Ref('c')
    *   'c' = Ref('a')
    *
    * If we are evaluating 'a', this would lead us to:

    *   eval(Ref('b'), Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *
    * and following the recursive pattern matching:
    *
    *   eval(SigB(),   Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(Ref('c'), Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(SigC(),   Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(Ref('a'), Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(SigA(),   Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(Ref('b'), Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *
    *  which would be an infinite loop.
    *
    *  To avoid the infinite loop we eliminate the reference evaluation from
    *  the reference map before calling the recursion:
    *
    *   eval(Ref('b'), Map('a' -> SigA, 'b' -> SigB, 'c' -> SigC))
    *   eval(SigB(),   Map('a' -> SigA, 'c' -> SigC))
    *   eval(Ref('c'), Map('a' -> SigA, 'c' -> SigC))
    *   eval(SigC(),   Map('a' -> SigA))
    *   eval(Ref('a'), Map('a' -> SigA))
    *   eval(SigA(),   Map.empty)
    *   eval(Ref('b'), Map.empty)  ===> ERROR: Cannot find 'b' in Map.empty
    *
    *
    *
    */
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case Ref(name) => {
        references.get(name) match {
          case Some(x) => eval(x(), references.filterKeys(_ != name))
          case None => Double.NaN
        }
      }
      case _ => Double.NaN
    }
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
