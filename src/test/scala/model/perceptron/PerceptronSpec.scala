/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.perceptron

import data.TestData.generateUnitBinaryClassificationExamples
import example.UnitBinaryClassificationExample
import org.scalatest.funsuite.AnyFunSuite

class PerceptronSpec extends AnyFunSuite {

  test("linearlySeparates() - positive case 1: 0 weights") {
    val examples = List(
      UnitBinaryClassificationExample(List(1.0, 1.0), 1),
      UnitBinaryClassificationExample(List(1.0, -1.0), -1)
    )
    assert(!Perceptron.linearlySeparates(List(0.0, 0.0, 0.0), examples))
  }

  test("linearlySeparates() - negative case 1: 2 points, 2 classes") {
    val examples = List(
      UnitBinaryClassificationExample(List(1.0, 1.0), 1),
      UnitBinaryClassificationExample(List(1.0, -1.0), -1)
    )
    assert(Perceptron.linearlySeparates(List(0.0, 0.0, 2.0), examples))
  }

  test("misclassifiedExamples() - case 1: 2 points, 1 misclassified") {
    val badEx = UnitBinaryClassificationExample(List(1.0, -1.0), -1)
    val examples = List(UnitBinaryClassificationExample(List(1.0, 1.0), 1), badEx)
    assert(Perceptron.misclassifiedExamples(List(0.0, 0.0, 0.0), examples) == List(badEx))
  }

  test("learn() - linearly separable case 1: 2 points, 2 classes") {
    val p = new Perceptron()
    val examples = List(
      new UnitBinaryClassificationExample(List(1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(1.0, -1.0), -1)
    )
    p.learn(examples)
    assert(p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("learn() - linearly separable case 2: 45 degree line") {
    val MinX = -10
    val MaxX = 10
    val p = new Perceptron()
    // Positive examples above 45 degree line, negative examples below
    val examples = (MinX to MaxX).toList.flatMap { n =>
      val exPos = new UnitBinaryClassificationExample(List(n, n + 1), 1)
      val exNeg = new UnitBinaryClassificationExample(List(n, n - 1), -1)
      List(exPos, exNeg)
    }
    p.learn(examples)
    assert(p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("learn() - linearly separable case 3: 3 dimensions") {
    val p = new Perceptron()
    val examples = List(
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), -1)
    )
    p.learn(examples)
    assert(p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("learn() - linearly separable case 4: only 1 class") {
    val p = new Perceptron()
    val examples = List(
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), 1)
    )
    p.learn(examples)
    assert(p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("learn() - linearly inseparable case 1: 2 same points, 2 classes") {
    val p = new Perceptron()
    val examples = List(
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), -1)
    )
    p.learn(examples)
    assert(!p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).length == 1)
  }

  test("learn() - linear inseparable case 1: 1 outlier") {
    val p = new Perceptron()
    val examples = List(
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(1.0, 1.0, 1.0), 1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), -1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), -1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), -1),
      new UnitBinaryClassificationExample(List(-1.0, -1.0, -1.0), 1)  // outlier
    )
    p.learn(examples)
    assert(!p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).length == 1)
  }

  test("learn() - linearly inseparable case 2: several outliers") {
    val TestSize = 50
    val NumOutliers = 3

    val p = new Perceptron()
    val examples = generateUnitBinaryClassificationExamples(TestSize, NumOutliers)
    p.learn(examples)
    assert(!p.linearlySeparates(examples))
    assert(p.misclassifiedExamples(examples).length == NumOutliers * 2)
  }

  test("inference() - case 1: 3 dimensions, positive label") {
    val p = new Perceptron()
    p.weights = List(1.0, 2.0, 3.0)
    val prediction = p.inference(List(1, 1))
    assert(prediction == 1)
  }

  test("inference() - case 2: 3 dimensions, negative label") {
    val p = new Perceptron()
    p.weights = List(-1.0, -2.0, -3.0)
    val prediction = p.inference(List(1, 1))
    assert(prediction == 0)
  }

  test("predictBatch() - case 1: positive label, negative label") {
    val p = new Perceptron()
    p.weights = List(1.0, 2.0, 3.0)
    val prediction = p.predictBatch(List(List(1, 1), List(-1, -1)))
    assert(prediction == List(1, 0))
  }

}