package ru.ifmo.ctddev.semenov.mathlogic

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.Deduction

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task2 {
  def main(args: Array[String]) {
    val (in, out) = if (args.length < 2) (System.in, System.out)
    else (Files.newInputStream(Paths.get(args(0))), Files.newOutputStream(Paths.get(args(1))))
    val parser = new PropositionalParser()
    val input = Source.fromInputStream(in).getLines().toArray
    val (header, proofLines) = (input.head, input.tail)
    val sep = header.lastIndexOf("|-")
    val assumptions = header.substring(0, sep).split("\\s+|,").filter(_.nonEmpty).map(parser.parse)
    val beta = parser parse header.substring(sep + 2)
    val writer = new PrintWriter(out)
    try {
      if (beta != (parser parse proofLines.last)) {
        writer println "Исходное доказательство некорректно. (Должно заканчиваться строкой '" + beta + "')"
      } else {
        val baseProof = proofLines map parser.parse
        Deduction(assumptions, baseProof) match {
          case None        =>
            writer println "Исходное доказательство некорректно."
          case Some(proof) =>
            val newAssumptions = assumptions.init
            if (!newAssumptions.isEmpty) {
              writer print newAssumptions.head
              newAssumptions.tail foreach (e => writer print s",$e")
            }
            writer print "|-"
            writer println s"(${assumptions.last})->$beta"
            proof foreach writer.println
        }
      }
    } finally {
      writer.close()
    }
  }
}
