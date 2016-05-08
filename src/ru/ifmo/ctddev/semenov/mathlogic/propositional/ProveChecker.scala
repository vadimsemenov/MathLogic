package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.{->, Expression}

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object ProveChecker {
  final val CORRECT = -1

  // TODO: create logger
  private def log(msg: String) = Console.err.println(msg)

  def check(prove: Iterator[Expression]): Int = {
    val rights = new mutable.HashMap[Expression, mutable.MutableList[Int]]()
    val proved = new mutable.ArrayBuffer[Expression]()
    val provedIdx = new mutable.HashMap[Expression, Int]()

    var index = 0
    for (expression <- prove) {
      val axiomIdx = Axioms.getIdx(expression)
      if (axiomIdx < 0) {
        rights.get(expression) match {
          case Some(list) =>
            var isOk = false
            for (snd <- list) {
              if (!isOk) {
                val exp = proved(snd).asInstanceOf[->]
                provedIdx.get(exp.lhs) match {
                  case Some(fst) =>
                    isOk = true
                    log(s"M.P. ${fst + 1},${snd + 1}")
                  case None      =>
                }
              }
            }
            if (!isOk) {
              log(s"Доказательство некорректно начиная с номера ${index + 1}")
              return index
            }
          case None       =>
            log(s"Доказательство некорректно начиная с номера ${index + 1}")
            return index
        }
      } else {
        log(s"Сх. акс. ${axiomIdx + 1}")
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
    CORRECT
  }
}
