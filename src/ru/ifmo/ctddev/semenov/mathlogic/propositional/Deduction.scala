package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Deduction {
  def apply(assumption: mutable.IndexedSeq[Expression], prove: mutable.IndexedSeq[Expression]): Option[mutable.ArrayBuffer[Expression]] = {
    val certificate = ProofChecker.getCertificate(assumption, prove)
    val alpha = assumption.last
    val proveSize = prove.size
    if (certificate.size != proveSize) return None
    val newProve = new mutable.ArrayBuffer[Expression](proveSize * 3 + 4) // upper bound
    var wasAlphaAlpha = false
    for (idx <- certificate.indices) certificate(idx) match {
      case (_, -1) | (-1, _) => // axiom or assumption
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
      case (fst, snd)        => // M.P. fst,snd
        val di = prove(idx)
        val dj = prove(fst)
        newProve += (alpha -> dj) -> ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += alpha -> di
    }
    Console.err.println("assumptions:")
    assumption.init.foreach(Console.err.println)
    Console.err.println("proof:")
    newProve.foreach(Console.err.println)
    assert(ProofChecker.check(assumption.init, newProve).isCorrect)
    Some(newProve)
  }
}
