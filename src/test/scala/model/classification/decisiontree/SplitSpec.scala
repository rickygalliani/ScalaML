/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import org.scalatest.funsuite.AnyFunSuite

class SplitSpec extends AnyFunSuite {

  val Delta = 10e-6

  test("numPos: case 1") {
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = 1)
    assert(s.numPos == 2)
  }

  test("size: case 1") {
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = 1)
    assert(s.size == 2)
  }

  test("leftYHat: case 1") {
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = 1)
    assert(math.abs(s.leftYHat - 1.0) < Delta)
  }

  test("rightYHat: case 1") {
    val rightSize = 4
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = rightSize)
    assert(math.abs(s.rightYHat - 0.25) < Delta)
  }

  test("yHat: case 1") {
    val rightSize = 4
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = rightSize)
    assert(math.abs(s.yHat - 0.4) < Delta)
  }

  test("getEntropy(): case 1 (perfect split)") {
    val s = Split(featureIndex = 0, threshold = 0.0, leftPos = 1, rightPos = 1, leftSize = 1, rightSize = 1)
    val entropyTest = s.getEntropy
    assert(entropyTest == 0.0)
  }

}
