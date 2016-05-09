package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.{Correct, Incorrect, ProofChecker}

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task1 {
  def main(args: Array[String]) {
    val parser = new PropositionalParser()
    val expressions = Source.fromInputStream(System.in).getLines().toIterable.map(parser.parse)
    val verdict = ProofChecker.check(expressions)
    println(verdict match {
      case Correct          => "Доказательство корректно."
      case Incorrect(index) => s"Доказательство некоректно начиная с номера ${index + 1}"
    })
  }
}
