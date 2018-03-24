package ru.ifmo.ctddev.semenov.mathlogic

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.parsing.FormalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task4 {
  def main(args: Array[String]) {
    val (in, out) = if (args.length < 2) (System.in, System.out)
    else (Files.newInputStream(Paths.get(args(0))), Files.newOutputStream(Paths.get(args(1))))
    val parser = new FormalParser()
    val input = ArrayBuffer(Source.fromInputStream(in).getLines().filter(_.nonEmpty).toList: _*)
    val (header, proofLines) = (input.head, input.tail)
    val (assumptions, beta) = parser parseHeader header
    val derivation = (assumptions, proofLines map parser.parse)
    val writer = new PrintWriter(out)
    try {
      ProofChecker.check(derivation, checkReducible = true) match {
        case Correct          =>
          if (assumptions.isEmpty) {
            writer println header
            proofLines foreach writer.println
          } else {
            Deduction(derivation) match {
              case Some((newContext, newProof)) =>
                writer println s"${newContext.mkString(",")}|-${newProof.last}"
                newProof foreach writer.println
              case None                         => writer println "POLUNDRA"
            }
          }
        case Incorrect(index, annotation) =>
          writer println s"Вывод некорректен начиная с формулы номер $index${
            if (annotation != null) ": " + annotation
            else ""
          }"
      }
    } finally {
      writer.close()
    }
  }
}
