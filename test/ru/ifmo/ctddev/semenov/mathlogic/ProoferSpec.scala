package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.propositional.Proofer

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class ProoferSpec extends FromFileSpec {

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

  def test(fileName: String): Proofer.Verdict = Proofer prove parseExpression(fileName, propositional = true)
}
