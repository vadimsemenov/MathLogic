package ru.ifmo.ctddev.semenov.mathlogic

import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.propositional.ProofChecker
import ru.ifmo.ctddev.semenov.mathlogic.utils.IOUtils

import scala.collection.mutable.ArrayBuffer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task1 {
  def main(args: Array[String]) {
    val (in, out) = if (args.length < 2) (System.in, System.out)
                    else (Files.newInputStream(Paths.get(args(0))), Files.newOutputStream(Paths.get(args(1))))
    val proof = IOUtils.readExpressions(in)
    val annotatedProof = ProofChecker.annotate(ArrayBuffer(proof: _*))
    IOUtils.write(out, annotatedProof)
  }
}
