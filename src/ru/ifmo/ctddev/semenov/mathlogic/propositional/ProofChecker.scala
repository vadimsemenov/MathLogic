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

  def getCertificate(assumptions: mutable.IndexedSeq[Expression], proof: Iterable[Expression]): ArrayBuffer[(Int, Int)] = {
    val certificate = new ArrayBuffer[(Int, Int)](proof.size)
    val rights = new mutable.HashMap[Expression, mutable.MutableList[Int]]()
    val proved = new mutable.ArrayBuffer[Expression]()
    val provedIdx = new mutable.HashMap[Expression, Int]()
    val assumptionIdx = new mutable.HashMap[Expression, Int]()
    for (index <- assumptions.indices) {
      assumptionIdx.put(assumptions(index), index)
    }

    var index = 0
    for (expression <- proof) {
      val axiomIdx = Axioms.getIdx(expression)
      if (axiomIdx < 0) {
        assumptionIdx get expression match {
          case Some(idx) =>
            log(s"Assumption #${idx + 1}")
            certificate += ((-1, idx))
          case None      => rights get expression match {
            case Some(list) =>
              var isOk = false
              for (snd <- list; if !isOk) {
                val exp = proved(snd).asInstanceOf[->]
                provedIdx get exp.lhs match {
                  case Some(fst) =>
                    isOk = true
                    log(s"M.P. ${fst + 1},${snd + 1}")
                    certificate += ((fst, snd))
                  case None      =>
                }
              }
              if (!isOk) {
                log(s"The proof is incorrect starting from #${index + 1}")
                return certificate
              }
            case None       =>
              log(s"The proof is incorrect starting from #${index + 1}")
              return certificate
          }
        }
      } else {
        log(s"Axiom schema #${axiomIdx + 1}")
        certificate += ((axiomIdx, -1))
      }
      proved += expression
      provedIdx.put(expression, index)
      expression match {
        case casted: -> =>
          val optionList = rights.get(casted.rhs)
          var list: mutable.MutableList[Int] = null
          if (optionList.isEmpty) {
            list = new mutable.MutableList[Int]
            rights.put(casted.rhs, list)
          } else {
            list = optionList.get
          }
          list += index
        case _          =>
      }
      index += 1
    }
    certificate
  }

  def check(assumption: mutable.IndexedSeq[Expression], proof: Iterable[Expression]): Verdict = {
    val length = getCertificate(assumption, proof).size
    if (length == proof.size) Correct
    else Incorrect(length)
  }

  def check(proof: Iterable[Expression]): Verdict = check(mutable.IndexedSeq.empty, proof)
}

trait Verdict {
  def isCorrect: Boolean = false
  def getFirstIncorrect: Int = throw new IllegalStateException("The proof is correct")
}

case object Correct extends Verdict {
  override def isCorrect = true
}

case class Incorrect(index: Int) extends Verdict {
  override def getFirstIncorrect = index
}