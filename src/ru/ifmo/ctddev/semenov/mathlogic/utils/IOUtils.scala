package ru.ifmo.ctddev.semenov.mathlogic.utils

import java.io.{InputStream, OutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression
import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object IOUtils {
  def write(writer: PrintWriter, iterable: Iterable[Any]): Unit =
    iterable foreach writer.println

  def write(outputStream: OutputStream, iterable: Iterable[Any]): Unit = {
    val writer = new PrintWriter(outputStream)
    try write(writer, iterable)
    finally writer.close()
  }

  private val propositionalParser = new PropositionalParser()

  def readExpressions(input: InputStream): List[Expression] =
    Source.fromInputStream(input).getLines().map(propositionalParser.parse).toList

  def readExpressions(fileName: String): List[Expression] =
    readExpressions(Files.newInputStream(Paths.get(fileName)))
}
