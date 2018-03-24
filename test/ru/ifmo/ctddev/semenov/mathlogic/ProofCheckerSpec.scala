package ru.ifmo.ctddev.semenov.mathlogic

import ru.ifmo.ctddev.semenov.mathlogic.propositional.{ProofChecker, Verdict}

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class ProofCheckerSpec extends FromFileSpec {
  "Task2" should "be correct" in {
    verdict("resources/HW2/contra.in", propositional = true).isCorrect shouldBe true
    verdict("resources/HW2/contra1.in", propositional = true).isCorrect shouldBe true
    verdict("resources/HW2/contra2.in", propositional = true).isCorrect shouldBe true
  }

  "Task4" should "be correct" in {
    verdict("resources/HW4/correct1.in").isCorrect shouldBe true
    verdict("resources/HW4/correct2.in").isCorrect shouldBe true
    verdict("resources/HW4/correct5.in").isCorrect shouldBe true
    verdict("resources/HW4/correct6.in").isCorrect shouldBe true
    verdict("resources/HW4/correct7.in").isCorrect shouldBe true
    verdict("resources/HW4/correct8.in").isCorrect shouldBe true
    verdict("resources/HW4/correct9.in").isCorrect shouldBe true
    verdict("resources/HW4/correct10.in").isCorrect shouldBe true
    verdict("resources/HW4/correct11.in").isCorrect shouldBe true
    verdict("resources/HW4/correct12.in").isCorrect shouldBe true
    verdict("resources/HW4/correct13.in").isCorrect shouldBe true
    verdict("resources/HW4/correct14.in").isCorrect shouldBe true
    verdict("resources/HW4/correct15.in").isCorrect shouldBe true
  }

  "Task4" should "not be correct" in {
    verdict("resources/HW4/incorrect1.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect2.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect3.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect4.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect5.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect6.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect7.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect8.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect9.in").isCorrect shouldBe false
    verdict("resources/HW4/incorrect10.in").isCorrect shouldBe false
//    verdict("resources/HW4/incorrect11.in").isCorrect shouldBe false
  }

  "Task4 p.2" should "be correct" in {
    verdict("resources/HW4/test7.out").isCorrect shouldBe true
  }

  "Task4 p.2" should "not be correct" in {
    verdict("resources/HW4/test11.out").isCorrect shouldBe false
    verdict("resources/HW4/test13.out").isCorrect shouldBe false
  }

  private def verdict(fileName: String, withHeader: Boolean = true, propositional: Boolean = false): Verdict =
    ProofChecker check (parseDerivation(fileName, withHeader, propositional), checkReducible = true)
}
