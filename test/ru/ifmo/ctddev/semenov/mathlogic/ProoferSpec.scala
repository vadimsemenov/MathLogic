package ru.ifmo.ctddev.semenov.mathlogic

import org.scalatest.{FlatSpec, Matchers}
import ru.ifmo.ctddev.semenov.mathlogic.parsing.PropositionalParser
import ru.ifmo.ctddev.semenov.mathlogic.propositional.Proofer

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class ProoferSpec extends FlatSpec with Matchers {
  def test(fileName: String): Proofer.Verdict = Proofer prove
    (new PropositionalParser() parse Source.fromFile(fileName, "UTF-8").mkString)

  it should "not be tautology" in {
    test("resources/HW3/false1.in").isTautology shouldBe false
  }

  it should "be provable tautology" in {
    test("resources/HW3/true1.in").isTautology shouldBe true
    test("resources/HW3/true2.in").isTautology shouldBe true
    test("resources/HW3/true3.in").isTautology shouldBe true
    test("resources/HW3/true4.in").isTautology shouldBe true
    test("resources/HW3/true5.in").isTautology shouldBe true
    test("resources/HW3/true6.in").isTautology shouldBe true
    test("resources/HW3/true7.in").isTautology shouldBe true
  }
}
