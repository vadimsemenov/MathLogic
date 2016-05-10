package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Proofer {
  def prove(expression: Expression): Option[List[Expression]] = None

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
