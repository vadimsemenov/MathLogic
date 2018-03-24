package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{->, _}

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Deduction {
  def apply(derivation: Derivation): Option[Derivation] = {
    val (assumption, proof) = derivation
    val annotatedProof = ProofChecker.annotate(derivation)
    val alpha = assumption.last
    val proveSize = proof.size
    val newProve = new mutable.ArrayBuffer[Expression](proveSize * 3 + 4) // upper bound
    var wasAlphaAlpha = false
    for (AnnotatedExpression(_, exp, annotation) <- annotatedProof) annotation match {
      case LogicAxiom(_) | PeanoAxiom(_) | InductionAxiom() | Assumption(_) =>
        if (exp != alpha) {
          newProve += exp
          newProve += exp -> (alpha -> exp)
          newProve += alpha -> exp
        } else if (!wasAlphaAlpha) {
          wasAlphaAlpha = true
          newProve += alpha -> (alpha -> alpha)
          newProve += (alpha -> (alpha -> alpha)) -> ((alpha -> ((alpha -> alpha) -> alpha)) -> (alpha -> alpha))
          newProve += (alpha -> ((alpha -> alpha) -> alpha)) -> (alpha -> alpha)
          newProve += alpha -> ((alpha -> alpha) -> alpha)
          newProve += alpha -> alpha
        }
      case ModusPonens(j, _)        =>
        val di = exp
        val dj = proof(j - 1)
        newProve += (alpha -> dj) -> ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += ((alpha -> (dj -> di)) -> (alpha -> di))
        newProve += alpha -> di
      case p: PredicateAxiom =>
        val arrow = exp.asInstanceOf[->]
        if (p.isUniversalAxiom) {
          val (@@(x, phi) -> psy) = arrow
          if (Matcher.matchWithOneSubstitution(phi, x)(psy).isEmpty) return None
        } else {
          val (phi -> ??(x, psy)) = arrow
          if (Matcher.matchWithOneSubstitution(psy, x)(phi).isEmpty) return None
        }
        newProve += exp
        newProve += exp -> (alpha -> exp)
        newProve += (alpha -> exp)
      case UniversalRule(_) =>
        val (phi -> @@(x, psy)) = exp.asInstanceOf[->]
        newProve ++= implicationToConjunction(alpha, phi, psy)
        newProve += (alpha & phi) -> psy
        newProve += (alpha & phi) -> @@(x, psy)
        newProve ++= conjunctionToImplication(alpha, phi, @@(x, psy))
        newProve += alpha -> exp
      case ExistentialRule(_) =>
        val (??(x, phi) -> psy) = exp.asInstanceOf[->]
        newProve ++= implicationReverse(alpha, phi, psy)
        newProve += phi -> (alpha -> psy)
        newProve += ??(x, phi) -> (alpha -> psy)
        newProve ++= implicationReverse(??(x, phi), alpha, psy)
        newProve += alpha -> exp
      case NotProved                =>
        return None
    }
    if (newProve.last != (alpha -> proof.last)) newProve += alpha -> proof.last // dirty hack to prove α → α
    val newDerivation: Derivation = Derivation(assumption.init, newProve)
    assert(ProofChecker.check(newDerivation).isCorrect)
    Some(newDerivation)
  }
}
