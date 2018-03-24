package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions._
import ru.ifmo.ctddev.semenov.mathlogic.expressions.Matcher._
import ru.ifmo.ctddev.semenov.mathlogic.formal.PeanoAxioms

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object ProofChecker {
  // TODO: create logger
  private def log(msg: String) = Console.err.println(msg)

  def annotate(derivation: Derivation, checkReducible: Boolean = false): ArrayBuffer[AnnotatedExpression] = {
    val (assumptions, proof) = derivation
    val annotated = new ArrayBuffer[AnnotatedExpression](proof.size)
    val lefts  = new mutable.HashMap[Expression, mutable.MutableList[Int]]()
    val rights = new mutable.HashMap[Expression, mutable.MutableList[Int]]()
    val provedIdx = new mutable.HashMap[Expression, Int]()
    val idxByAssumption = new mutable.HashMap[Expression, Int]()
    for (index <- assumptions.indices) {
      idxByAssumption.put(assumptions(index), index)
    }
    val reducibleFv = if (checkReducible && assumptions.nonEmpty) freeVars(assumptions.last) else Set.empty[String]

    def asLogicAxiom(expression: Expression): Option[Annotation] = LogicAxioms getIdx expression map (_ + 1) map LogicAxiom
    def asPeanoAxiom(expression: Expression): Option[Annotation] = PeanoAxioms getIdx expression map (_ + 1) map PeanoAxiom
    def asAssumption(expression: Expression): Option[Annotation] = idxByAssumption get expression map (_ + 1) map Assumption
    val toUniversalAxiom: PartialFunction[Expression, Annotation] = {
      case @@(v, alpha) -> beta if matchWithOneSubstitution(alpha, v)(beta).isDefined =>
        val substitution = matchWithOneSubstitution(alpha, v)(beta) flatMap (_.get(v.name))
        /*if (reducibleFv contains v.name) Annotation.illegalBoundingAxiom(v.name, assumptions.last)
        else*/ if (substitution.isDefined && !freeForSubstitution(alpha, v.name, substitution.get)) NotFreeForSubstitution(alpha, v.name, beta)
        else Annotation.universalAxiom
    }
    val toExistentialAxiom: PartialFunction[Expression, Annotation] = {
      case alpha -> ??(v, beta) if matchWithOneSubstitution(beta, v)(alpha).isDefined =>
        val substitution = matchWithOneSubstitution(beta, v)(alpha) flatMap (_.get(v.name))
        /*if (reducibleFv contains v.name) Annotation.illegalBoundingAxiom(v.name, assumptions.last)
        else*/ if (substitution.isDefined && !freeForSubstitution(beta, v.name, substitution.get)) NotFreeForSubstitution(beta, v.name, alpha)
        else Annotation.existentialAxiom
    }
    def asPredicateAxiom(expression: Expression): Option[Annotation] = toUniversalAxiom orElse toExistentialAxiom lift expression
    def asUniversalRule(expression: Expression): Option[Annotation] = expression match {
      case alpha -> @@(v, beta) =>
        val matches = matchWithOneSubstitution(beta, v)(_)
        lefts get alpha flatMap (_ collectFirst {
          case idx: Int if matches(proof(idx).asInstanceOf[->].rhs).isDefined =>
//            val substitution = matches(proof(idx).asInstanceOf[->].rhs) flatMap (_.get(v.name))
            if (reducibleFv contains v.name) Annotation.illegalBoundingRule(v.name, assumptions.last)
            else if (freeVars(alpha) contains v.name) IllegalQuantifierIntroduction(v.name, alpha)
//            else if (substitution.isDefined && !freeForSubstitution(beta, v.name, substitution.get)) NotFreeForSubstitution(expression, v.name, substitution.get)
            else UniversalRule(idx)
        })
      case _                    => None
    }
    def asExistentialRule(expression: Expression): Option[Annotation] = expression match {
      case ??(v, alpha) -> beta =>
        val matches = matchWithOneSubstitution(alpha, v)(_)
        rights get beta flatMap (_ collectFirst {
          case idx: Int if matches(proof(idx).asInstanceOf[->].lhs).isDefined =>
//            val substitution = matches(proof(idx).asInstanceOf[->].lhs) flatMap  (_.get(v.name))
            if (reducibleFv contains v.name) Annotation.illegalBoundingRule(v.name, assumptions.last)
            else if (freeVars(beta) contains v.name) IllegalQuantifierIntroduction(v.name, alpha)
//            else if (substitution.isDefined && !freeForSubstitution(alpha, v.name, substitution.get)) NotFreeForSubstitution(alpha, v.name, substitution.get)
            else ExistentialRule(idx)
        })
      case _                    => None
    }
    val toInductionAxiom: PartialFunction[Expression, Annotation] = {
      case (alpha & @@(x, base -> next)) -> beta if matchAfterSubstitution(base, x, x.succ)(next)
        && matchAfterSubstitution(beta, x, Zero())(alpha) => InductionAxiom()
    }
    def asInductionAxiom(expression: Expression): Option[Annotation] = toInductionAxiom lift expression
    def asModusPonens(expression: Expression): Option[Annotation] = rights get expression flatMap { list =>
      list.toStream map (idx => (idx, proof(idx))) collectFirst {
        case (idx: Int, (lhs -> _)) if provedIdx contains lhs => ModusPonens(provedIdx(lhs) + 1, idx + 1)
      }
    }

    def createAnnotation(expression: Expression): Annotation = asLogicAxiom(expression)
      .orElse(asPeanoAxiom(expression))
      .orElse(asInductionAxiom(expression))
      .orElse(asAssumption(expression))
      .orElse(asPredicateAxiom(expression))
      .orElse(asModusPonens(expression))
      .orElse(asUniversalRule(expression))
      .orElse(asExistentialRule(expression))
      .getOrElse(NotProved)

    var failed = false
    for ((expression, index) <- proof.zipWithIndex) {
      val annotation = if (failed) NotProved else createAnnotation(expression)

      if (!failed) {
        if (!annotation.isProved) {
          log(s"The proof is incorrect starting from line #${index + 1}")
          failed = true
        } else {
          provedIdx.put(expression, index)
          expression match {
            case (lhs -> rhs) =>
              rights.getOrElseUpdate(rhs, new mutable.MutableList[Int]) += index
              lefts .getOrElseUpdate(lhs, new mutable.MutableList[Int]) += index
            case _          =>
          }
        }
      }
      annotated += AnnotatedExpression(index, expression, annotation)
    }
    annotated
  }

  def annotate(proof: Proof): AnnotatedProof =
    annotate((mutable.ArrayBuffer.empty[Expression], proof))

  def check(derivation: Derivation, checkReducible: Boolean = false): Verdict = {
    val annotatedProof = annotate(derivation, checkReducible)
    if (annotatedProof.last.annotation.isProved) Correct
    else {
      val idx = annotatedProof.indexWhere(e => !e.annotation.isProved)
      Incorrect(1 + idx, annotatedProof(idx).annotation)
    }
  }

  def check(proof: Proof): Verdict = check(Derivation(mutable.ArrayBuffer.empty, proof))
}

sealed trait Verdict {
  def isCorrect: Boolean = false
  def getFirstIncorrect: Int = throw new IllegalStateException("The proof is correct")
}

case object Correct extends Verdict {
  override def isCorrect = true
}

case class Incorrect(index: Int, annotation: Annotation = null) extends Verdict {
  override def getFirstIncorrect = index
}

/*
val asLogicAxiom: PartialFunction[Expression, Annotation] = unlift(LogicAxioms.getIdx) andThen (_ + 1) andThen LogicAxiom
    val asPeanoAxiom: PartialFunction[Expression, Annotation] = unlift(PeanoAxioms.getIdx) andThen (_ + 1) andThen PeanoAxiom
    val asAssumption: PartialFunction[Expression, Annotation] = idxByAssumption andThen (_ + 1) andThen Assumption
    val isUniversalAxiom: PartialFunction[Expression, Annotation] = {
      case @@(v, alpha) -> beta if MatcherWithSubstitution(alpha, v)(beta) => Annotation.universalAxiom
    }
    val isExistentialAxiom: PartialFunction[Expression, Annotation] = {
      case alpha -> ??(v, beta) => ???
    }
    val asPredicateAxiom: PartialFunction[Expression, Annotation] = isUniversalAxiom orElse isExistentialAxiom
    val asUniversalRule: PartialFunction[Expression, Annotation] = ???
    val asExistentialRule: PartialFunction[Expression, Annotation] = ???
    val asInductionRule: PartialFunction[Expression, Annotation] = ???
    val asModusPonens: PartialFunction[Expression, Annotation] = rights andThen
      unlift(_.toStream
        .filter(i => proof(i).isInstanceOf[->])
        .map(i =>
          provedIdx.get(proof(i).asInstanceOf[->].lhs)
            .map(j => ModusPonens(i + 1, j + 1))
        )
        .head
      )
//      rights get expression flatMap { list =>
//      list.toStream.flatMap(snd => {
//        val expr = proof(snd).asInstanceOf[->]
//        provedIdx.get(expr.lhs).map(fst => ModusPonens(fst + 1, snd + 1))
//      }).headOption
//    }

    def createAnnotation(expression: Expression): Annotation = asLogicAxiom orElse
      asPeanoAxiom orElse
      asAssumption orElse
      asModusPonens orElse
      asPredicateAxiom orElse
      asUniversalRule orElse
      asExistentialRule orElse
      asInductionRule applyOrElse(expression, _ => NotProved)

 */