package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Task1 extends App {
  val parser = new PropositionalParser()
  println(parser.parse("(A->A->A)->(A->(A->A)->A)->(A->A)"))
  println(parser.parse("!B->B->!B"))
  println(parser.parse("(B->(!A->B))->(B->(!A->B)->((!A->!B)->!!A))->B->((!A->!B)->!!A)"))
}
