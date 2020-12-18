/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import model.classification.decisiontree.DecisionTreeClassifier.{entropy, weightedEntropy}
import org.scalatest.funsuite.AnyFunSuite

class DecisionTreeClassifierSpec extends AnyFunSuite {

  val Delta = 10e-6

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
