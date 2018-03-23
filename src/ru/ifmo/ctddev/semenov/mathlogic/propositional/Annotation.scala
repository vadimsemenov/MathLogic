package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.formal.PeanoAxioms

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Annotation {
  def isProved: Boolean = true
  def isLogicAxiom: Boolean = false
  def isPredicateAxiom: Boolean = false // about quantifiers
  def isPeanoAxiom: Boolean = false
  def isAssumption: Boolean = false
  def isModusPonens: Boolean = false
  def isUniversalRule: Boolean = false
  def isExistentialRule: Boolean = false
}

case class LogicAxiom(num: Int) extends Annotation {
  require(1 <= num && num <= LogicAxioms.axioms.size, { num })
  override def isLogicAxiom = true
  override def toString = s"Сх. акс. $num"
}

case class PredicateAxiom(num: Int) extends Annotation {
  require(1 <= num && num <= 2)
  override def isPredicateAxiom: Boolean = true
  override def toString: String = s"Сх. акс. ${if (num == 1) "всеобщности" else "существования"}"
}

case class PeanoAxiom(num: Int) extends Annotation {
  require(1 <= num && num <= PeanoAxioms.axioms.size)
  override def isPeanoAxiom: Boolean = true
  override def toString: String = s"Сх. акс. Пеано $num"
}

case class Assumption(num: Int) extends Annotation {
  override def isAssumption = true
  override def toString = s"Гипотеза $num"
}

case class ModusPonens(antecedentId: Int, implicationId: Int) extends Annotation {
  override def isModusPonens = true
  override def toString = s"M.p. $antecedentId, $implicationId"
}

case class UniversalRule(baseId: Int) extends Annotation {
  override def isUniversalRule: Boolean = true
  override def toString: String = s"Правило вывода для всеобщности из $baseId"
}

case class ExistentialRule(baseId: Int) extends Annotation {
  override def isExistentialRule: Boolean = true
  override def toString: String = s"Правило вывода для существования из $baseId"
}

case object NotProved extends Annotation {
  override def isProved = false
  override def toString = "Не доказано"
}