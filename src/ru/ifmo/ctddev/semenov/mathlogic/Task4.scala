package ru.ifmo.ctddev.semenov.mathlogic

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.parsing.FormalParser

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
    val input = ArrayBuffer(Source.fromInputStream(in).getLines().toList: _*)
    val (header, proofLines) = (input.head, input.tail)
    val (assumptions, beta) = parser parseHeader header
    val writer = new PrintWriter(out)
    try {
//      if (beta != (parser parse proofLines.last)) {
//        writer println "Исходное доказательство некорректно. (Должно заканчиваться строкой '" + beta + "')"
//      } else {
        writer println s"${assumptions.mkString(",")}|-$beta"
        // TODO
//      }
    } finally {
      writer.close()
    }
  }
}
