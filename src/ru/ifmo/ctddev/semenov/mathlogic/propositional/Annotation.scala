package ru.ifmo.ctddev.semenov.mathlogic.propositional

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Annotation {
  def isProved: Boolean = true
  def isAxiom: Boolean = false
  def isAssumption: Boolean = false
  def isModusPonens: Boolean = false
}

case class Axiom(num: Int) extends Annotation {
  override def isAxiom = true
  override def toString = s"Сх. акс. $num"
}

case class Assumption(num: Int) extends Annotation {
  override def isAssumption = true
  override def toString = s"Гипотеза $num"
}

case class ModusPonens(antecedentId: Int, implicationId: Int) extends Annotation {
  override def isModusPonens = true
  override def toString = s"M.p. $antecedentId, $implicationId"
}

case object NotProved extends Annotation {
  override def isProved = false
  override def toString = "Не доказано"
}