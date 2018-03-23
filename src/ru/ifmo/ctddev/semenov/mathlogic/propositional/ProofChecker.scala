package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{->, Expression}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object ProofChecker {
  // TODO: create logger
  private def log(msg: String) = Console.err.println(msg)

  def annotate(derivation: Derivation): ArrayBuffer[AnnotatedExpression] = {
    val (assumptions, proof) = derivation
    val annotated = new ArrayBuffer[AnnotatedExpression](proof.size)
    val rights = new mutable.HashMap[Expression, mutable.MutableList[Int]]()
    val provedIdx = new mutable.HashMap[Expression, Int]()
    val idxByAssumption = new mutable.HashMap[Expression, Int]()
    for (index <- assumptions.indices) {
      idxByAssumption.put(assumptions(index), index)
    }

    def asLogicAxiom(expression: Expression): Option[Annotation] = LogicAxioms getIdx expression map (LogicAxiom compose (_ + 1))

//    def asPeanoAxiom(expression: Expression): Option[Annotation] = PeanoAxioms getIdx expression map (PeanoAxiom compose (_ + 1))
    def asAssumption(expression: Expression): Option[Annotation] = idxByAssumption get expression map (Assumption compose (_ + 1))
    def asPredicateAxiom(expression: Expression): Option[Annotation] = ???
    def asModusPonens(expression: Expression): Option[Annotation] = rights get expression flatMap { list =>
      list.toStream.flatMap(snd => {
        val expr = proof(snd).asInstanceOf[->]
        provedIdx.get(expr.lhs).map(fst => ModusPonens(fst + 1, snd + 1))
      }).headOption
    }

    def createAnnotation(expression: Expression): Annotation = asLogicAxiom(expression)
//      .orElse(asPeanoAxiom(expression))
      .orElse(asAssumption(expression))
      .orElse(asModusPonens(expression))
      .getOrElse(NotProved)

    var failed = false
    for ((expression, index) <- proof.zipWithIndex) {
      val annotation = if (failed) NotProved else createAnnotation(expression)

      if (!failed) {
        if (annotation == NotProved) {
          log(s"The proof is incorrect starting from line #${index + 1}")
          failed = true
        } else {
          provedIdx.put(expression, index)
          expression match {
            case (_ -> rhs) => rights.getOrElseUpdate(rhs, new mutable.MutableList[Int]) += index
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

  def check(derivation: Derivation): Verdict = {
    val annotatedProof = annotate(derivation)
    if (annotatedProof.last.annotation != NotProved) Correct
    else Incorrect(1 + annotatedProof.indexWhere(_.annotation == NotProved))
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

case class Incorrect(index: Int) extends Verdict {
  override def getFirstIncorrect = index
}