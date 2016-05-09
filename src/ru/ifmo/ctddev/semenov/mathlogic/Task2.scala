package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.Deduction

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task2 {
  def main(args: Array[String]) {
    val parser = new PropositionalParser()
    val input = Source.fromInputStream(System.in).getLines().toArray
    val (header, proofLines) = (input.head, input.tail)
    val sep = header.lastIndexOf("|-")
    val assumptions = header.substring(0, sep).split("\\s+|,").filter(_.nonEmpty).map(parser.parse)
    val beta = header.substring(sep + 2).trim
    if (beta != proofLines.last.trim) {
      println("Исходное доказательство некорректно. (Должно заканчиваться строкой '" + beta + "')")
      return
    }
    val baseProof = proofLines.map(parser.parse)
    Deduction(assumptions, baseProof) match {
      case None        => println("Исходное доказательство некорректно.")
      case Some(proof) => proof foreach println
    }
  }
}
