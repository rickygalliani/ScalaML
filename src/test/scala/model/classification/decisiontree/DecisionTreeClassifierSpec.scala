/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import example.BinaryClassificationExample
import org.scalatest.funsuite.AnyFunSuite

class DecisionTreeClassifierSpec extends AnyFunSuite {

  val Delta = 10e-6

  test("learn(): case 1 (maxDepth: 1, 1 example)") {
    val examples = List[BinaryClassificationExample](
      new BinaryClassificationExample(X = List(0.0, 0.0, 0.0), y = 1.0),
    )
    val dt = new DecisionTreeClassifier(maxDepth = 1, verbose = false)
    dt.learn(examples)
    assert(math.abs(1.0 - dt.inference(List(0.0, 0.0, 0.0))) < Delta)
  }

  test("learn(): case 2 (maxDepth: 1, 2 examples)") {
    val examples = List[BinaryClassificationExample](
      new BinaryClassificationExample(X = List(0.0, 0.0, 0.0), y = 0.0),
      new BinaryClassificationExample(X = List(1.0, 0.0, 0.0), y = 1.0)
    )
    val dt = new DecisionTreeClassifier(maxDepth = 1, verbose = false)
    dt.learn(examples)
    assert(math.abs(0.5 - dt.inference(List(0.0, 0.0, 0.0))) < Delta)
  }

  test("learn(): case 2 (maxDepth: 10, 2 examples)") {
    val examples = List[BinaryClassificationExample](
      new BinaryClassificationExample(X = List(0.0, 0.0, 0.0), y = 0.0),
      new BinaryClassificationExample(X = List(1.0, 0.0, 0.0), y = 1.0)
    )
    val maxDepth = 10
    val dt = new DecisionTreeClassifier(maxDepth = maxDepth, verbose = false)
    dt.learn(examples)
    assert(math.abs(0.5 - dt.inference(List(0.0, 0.0, 0.0))) < Delta)
  }

}
