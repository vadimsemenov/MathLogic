package ru.ifmo.ctddev.semenov.mathlogic.parsing

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Utils {
  def isDigit(c: Char): Boolean      = '0' <= c && c <= '9'
  def isWhitespace(c: Char): Boolean = Character.isWhitespace(c)
}
