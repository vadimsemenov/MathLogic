package ru.ifmo.ctddev.semenov.mathlogic

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.{ProofChecker, Proofer}
import ru.ifmo.ctddev.semenov.mathlogic.propositional.Proofer.{NonTautology, Tautology}
import ru.ifmo.ctddev.semenov.mathlogic.utils.IOUtils

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task3 {
  def main(args: Array[String]) {
    val (in, out) = if (args.length < 2) (System.in, System.out)
    else (Files.newInputStream(Paths.get(args(0))), Files.newOutputStream(Paths.get(args(1))))
    val parser = new PropositionalParser()
    val input = ArrayBuffer(Source.fromInputStream(in).getLines().map(_.trim).filter(_.nonEmpty).toList: _*)
    if (input.length != 1) throw new IllegalStateException("Input should contain only one line with expression")
    val expression = parser parse input(0)
    val writer = new PrintWriter(out)
    try {
      Proofer.prove(expression) match {
        case NonTautology(assignment) =>
          assert(assignment.nonEmpty)
          def ats(assignment: (String, Boolean)) = s"${assignment._1}=${if (assignment._2) 'И' else 'Л'}"
          writer print "Высказывание ложно при "
          writer print ats(assignment.head)
          if (assignment.tail.nonEmpty) {
            writer print ","
            assignment.tail.foreach(a => writer print s",${ats(a)}")
          }
        case Tautology(proof)         =>
          assert(ProofChecker.check(proof).isCorrect, "Incorrect proof :(")
          IOUtils.write(writer, proof)
      }
    } finally {
      writer.close()
    }
  }
}
