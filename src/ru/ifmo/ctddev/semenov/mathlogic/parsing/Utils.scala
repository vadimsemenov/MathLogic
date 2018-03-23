package ru.ifmo.ctddev.semenov.mathlogic.parsing

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Utils {
  def isAlphabetic(c: Char): Boolean = isLowerCase(c) || isUpperCase(c)
  def isUpperCase(c: Char): Boolean  = 'A' <= c && c <= 'Z'
  def isLowerCase(c: Char): Boolean  = 'a' <= c && c <= 'z'
  def isDigit(c: Char): Boolean      = '0' <= c && c <= '9'
  def isWhitespace(c: Char): Boolean = Character.isWhitespace(c)
}
