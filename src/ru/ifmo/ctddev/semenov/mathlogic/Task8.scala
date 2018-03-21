package ru.ifmo.ctddev.semenov.mathlogic

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.ordinals.OrdinalParser

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task8 {
  def main(args: Array[String]): Unit = {
    val (in, out) = if (args.length < 2) (System.in, System.out)
    else (Files.newInputStream(Paths.get(args(0))), Files.newOutputStream(Paths.get(args(1))))
    val writer = new PrintWriter(out)
    try {
      scala.io.Source.fromInputStream(in).getLines().filter(_.nonEmpty).foreach(
        (input: String) => new OrdinalParser(input).equalityOrdinals.run().get match {
          case (lhs, rhs) =>
            Console.err.println(lhs)
            Console.err.println(rhs)
            writer.println(if (lhs == rhs) "Равны" else "Не равны")
        }
      )
    } finally {
      writer.close()
    }
  }
}
