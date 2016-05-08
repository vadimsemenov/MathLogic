package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.ProveChecker

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task1 {
  def main(args: Array[String]) {
    val parser = new PropositionalParser()
    val expressions = Source.fromInputStream(System.in).getLines().map(parser.parse)
    val verdict = ProveChecker.check(expressions)
    println(
      if (verdict == ProveChecker.CORRECT) "Доказательство корректно."
      else s"Доказательство некоректно начиная с номера ${verdict + 1}"
    )
  }
}
