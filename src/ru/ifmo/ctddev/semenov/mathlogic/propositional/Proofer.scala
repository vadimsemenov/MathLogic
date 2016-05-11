package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Proofer {
  private def eval(expression: Expression, mask: Int, map: mutable.HashMap[String, Int]): Boolean = {
    def eval(expression: Expression): Boolean = expression match {
      case ->(lhs, rhs)   => if (eval(lhs)) eval(rhs) else true
      case &(lhs, rhs)    => eval(lhs) && eval(rhs)
      case |(lhs, rhs)    => eval(lhs) || eval(rhs)
      case !(arg)         => !eval(arg)
      case Variable(name) => ((mask >> map(name)) & 1) != 0
      case _              => throw new AssertionError("cannot eval " + expression)
    }

    eval(expression)
  }

  private def getVarSet(expression: Expression): Set[String] = expression match {
    case ->(lhs, rhs)   => getVarSet(lhs) ++ getVarSet(rhs)
    case &(lhs, rhs)    => getVarSet(lhs) ++ getVarSet(rhs)
    case |(lhs, rhs)    => getVarSet(lhs) ++ getVarSet(rhs)
    case !(arg)         => getVarSet(arg)
    case Variable(name) => Set(name)
  }

  def checkTautology(expression: Expression): Option[List[(String, Boolean)]] = {
    val varSet = getVarSet(expression)
    val map = new mutable.HashMap[String, Int]()
    var shift = 0
    for (name <- varSet) {
      map.put(name, shift)
      shift += 1
    }
    val upTo = 1 << shift
    for (mask <- 0 until upTo) {
      if (!eval(expression, mask, map)) {
        var interpretation: List[(String, Boolean)] = List()
        for ((name, shift) <- map) {
          interpretation = (name, ((mask >>> shift) & 1) != 0) :: interpretation
        }
        return Some(interpretation)
      }
    }
    None
  }

  // TODO: extract to package, adjust with ProofChecker$Verdict
  trait Verdict {
    def isTautology: Boolean
    def getProof: List[Expression] = throw new IllegalStateException("Expression is not tautology")
    def getInterpretation: List[(String, Boolean)] = throw new IllegalStateException("Expression is tautology")
  }

  case class Tautology(proof: List[Expression]) extends Verdict {
    override def isTautology = true
    override def getProof = proof
  }

  case class NonTautology(interpretation: List[(String, Boolean)]) extends Verdict {
    override def isTautology = false
    override def getInterpretation = interpretation
  }

  def prove(expression: Expression): Verdict = checkTautology(expression) match {
    case Some(interpretation) => NonTautology(interpretation)
    case None                 =>
      val varSet = getVarSet(expression)
      // todo
      Tautology(List.empty)
  }

  /**
    * α → β, α ⊢ β
    */
  def modusPonens(alpha: Expression, beta: Expression): List[Expression] = List(beta)

  /**
    * α → β, ¬β ⊢ ¬α
    */
  def modusTollens(alpha: Expression, beta: Expression): List[Expression] = List(
    !beta -> (alpha -> !beta),
    alpha -> !beta,
    (alpha -> beta) -> ((alpha -> !beta) -> !alpha),
    (alpha -> !beta) -> !alpha,
    !alpha
  )

  def deduction(assumption: Iterable[Expression], proof: Iterable[Expression]): ArrayBuffer[Expression] = Deduction(
    (mutable.IndexedSeq.newBuilder ++= assumption).result(),
    (mutable.IndexedSeq.newBuilder ++= proof).result()) match {
      case Some(x) => x
      case None    => throw new AssertionError("impossible deduction")
  }

  /**
    * ⊢ (α | ¬α)
    */
  def tertiumNonDatur(alpha: Expression): List[Expression] = {
    def part(b: Expression): List[Expression] = {
      List(alpha -> b) ++ deduction(List(alpha -> b, !b), modusTollens(alpha, b))
    }
    part(alpha | !alpha) ++
      part(!(alpha | !alpha)) ++
      List(
        (!(alpha | !alpha) -> !alpha) -> (((alpha | !alpha) -> !(!alpha)) -> !(!(alpha | !alpha))),
        ((alpha | !alpha) -> !(!alpha)) -> !(!(alpha | !alpha)),
        !(!(alpha | !alpha)),
        !(!(alpha | !alpha)) -> (alpha | !alpha),
        alpha | !alpha
      )
  }
}
