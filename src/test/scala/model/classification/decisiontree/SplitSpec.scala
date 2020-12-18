/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import Split._
import example.BinaryClassificationExample
import org.scalatest.funsuite.AnyFunSuite

class SplitSpec extends AnyFunSuite {

  val Delta = 10e-6

  test("numPos: case 1") {
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, 1)
    assert(s.numPositives == 2)
  }

  test("size: case 1") {
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, 1)
    assert(s.size == 2)
  }

  test("leftYHat: case 1") {
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, 1)
    assert(math.abs(s.leftYHat - 1.0) < Delta)
  }

  test("rightYHat: case 1") {
    val rightSize = 4
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, rightSize)
    assert(math.abs(s.rightYHat - 0.25) < Delta)
  }

  test("yHat: case 1") {
    val rightSize = 4
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, rightSize)
    assert(math.abs(s.yHat - 0.4) < Delta)
  }

  test("getEntropy(): case 1 (perfect split)") {
    val examples = List[BinaryClassificationExample]()
    val s = Split(0, 0.0, examples, examples, leftPositives = 1, rightPositives = 1, 1, 1)
    val entropyTest = s.entropy
    assert(entropyTest == 0.0)
  }

  test("entropy(): case 1 - maximal entropy") {
    val entropyTest = entropy(0.5)
    assert(math.abs(entropyTest - 0.6931471805599453) < Delta)
  }

  test("entropy(): case 2 - entropy(0.0") {
    val entropyTest = entropy(0.0)
    assert(math.abs(entropyTest) < Delta)
  }

  test("entropy(): case 3 - entropy(1.0") {
    val entropyTest = entropy(1.0)
    assert(math.abs(entropyTest) < Delta)
  }

  test("weightedEntropy(): case 1 - leftEntropy = 0, rightEntropy = 0") {
    val weightedEntropyTest = weightedEntropy(1, 1, 0.0, 0.0)
    assert(math.abs(weightedEntropyTest) < Delta)
  }

  test("weightedEntropy(): case 2 - leftSize = 0") {
    val weightedEntropyTest = weightedEntropy(0, 1, 0.5, 0.5)
    assert(math.abs(weightedEntropyTest - 0.5) < Delta)
  }

}
