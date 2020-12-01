/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.votedperceptron

import org.scalatest.funsuite.AnyFunSuite
  
class VotedPerceptronSpec extends AnyFunSuite {

  test("inference() - case 1: 3 dimensions, positive label") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 1.0))
    val prediction = vp.inference(List(1, 1))
    assert(prediction == 1)
  }

  test("inference() - case 2: 3 dimensions, negative label") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(-1.0, -2.0, -3.0), 1.0))
    val prediction = vp.inference(List(1, 1))
    assert(prediction == 0)
  }

  test("inference() - case 3: 3 dimensions, 2 vectors") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 0.5), (List(2.0, 4.0, 6.0), 0.5))
    val prediction = vp.inference(List(1, 1))
    assert(prediction == 1)
  }

  test("predictBatch() - case 1: 2 dimensions 1 vector") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 1.0))
    val predictions = vp.predictBatch(List(List(1, 1), List(-1, -1)))
    assert(predictions == List(1, 0))
  }

  test("predictBatch() - case 2: 3 dimensions 2 vectors") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 0.5), (List(2.0, 4.0, 6.0), 0.5))
    val predictions = vp.predictBatch(List(List(1, 1), List(-1, -1)))
    assert(predictions == List(1, 0))
  }

}