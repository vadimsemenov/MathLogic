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
    val proved = new mutable.ArrayBuffer[Expression]()
    val provedIdx = new mutable.HashMap[Expression, Int]()
    val idxByAssumption = new mutable.HashMap[Expression, Int]()
    for (index <- assumptions.indices) {
      idxByAssumption.put(assumptions(index), index)
    }

    def createAnnotation(expression: Expression): Annotation = {
      Axioms.getIdx(expression) match {
        case None           => idxByAssumption get expression match {
          case Some(idx) =>
            Assumption(idx + 1)
          case None      => rights get expression match {
            case Some(list) =>
              for (snd <- list) {
                val exp = proved(snd).asInstanceOf[->]
                provedIdx get exp.lhs match {
                  case Some(fst) =>
                    //                    log(s"M.P. ${fst + 1},${snd + 1}")
                    return ModusPonens(fst + 1, snd + 1)
                  case None      =>
                }
              }
              NotProved
            case None       =>
              NotProved
          }
        }
        case Some(axiomIdx) =>
          //          log(s"Axiom schema #${axiomIdx + 1}")
          Axiom(axiomIdx + 1)
      }
    }

    var index = 0
    var failed = false
    for (expression <- proof) {
      val annotation = if (failed) NotProved else createAnnotation(expression)

      if (!failed) {
        if (annotation == NotProved) {
          log(s"The proof is incorrect starting from line #${index + 1}")
          failed = true
        } else {
          proved += expression
          provedIdx.put(expression, index)
          expression match {
            case casted: -> =>
              val list: mutable.MutableList[Int] = rights.getOrElse(casted.rhs, new mutable.MutableList[Int])
              if (list.isEmpty) rights.put(casted.rhs, list)
              list += index
            case _          =>
          }
        }
      }
      index += 1
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