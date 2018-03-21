package ru.ifmo.ctddev.semenov.mathlogic

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
package object ordinals {
  val ZERO = Atom(0)
  val ONE = Atom(1)
  val ω = CNF(ONE, 1, ZERO)
}
