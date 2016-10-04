package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Deduction {
  def apply(assumption: mutable.IndexedSeq[Expression], prove: mutable.IndexedSeq[Expression]): Option[mutable.ArrayBuffer[Expression]] = {
    val annotatedProof = ProofChecker.annotate(assumption, prove)
    val alpha = assumption.last
    val proveSize = prove.size
    val newProve = new mutable.ArrayBuffer[Expression](proveSize * 3 + 4) // upper bound
    var wasAlphaAlpha = false
    for (idx <- annotatedProof.indices) annotatedProof(idx).annotation match {
      case Axiom(_) | Assumption(_)  =>
        if (prove(idx) != alpha) {
          newProve += prove(idx)
          newProve += prove(idx) -> (alpha -> prove(idx))
          newProve += alpha -> prove(idx)
        } else if (!wasAlphaAlpha) {
          wasAlphaAlpha = true
          newProve += alpha -> (alpha -> alpha)
          newProve += (alpha -> (alpha -> alpha)) -> ((alpha -> ((alpha -> alpha) -> alpha)) -> (alpha -> alpha))
          newProve += (alpha -> ((alpha -> alpha) -> alpha)) -> (alpha -> alpha)
          newProve += alpha -> ((alpha -> alpha) -> alpha)
          newProve += alpha -> alpha
        }
      case ModusPonens(j, _) =>
        val di = prove(idx)
        val dj = prove(j - 1)
        newProve += (alpha -> dj) -> ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += alpha -> di
      case NotProved                 =>
        return None
    }
    assert(ProofChecker.check(assumption.init, newProve).isCorrect)
    Some(newProve)
  }
}
