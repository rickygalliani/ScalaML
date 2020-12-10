/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.logisticregression

import org.scalatest.funsuite.AnyFunSuite

class LogisticRegressionSpec extends AnyFunSuite {

  val Delta = 10e-6

  test("loss() - y = 1, yHat = 0.8") {
    val lossTest = LogisticRegression.loss(y = 1.0, yHat = 0.8)
    val lossTruth = 0.2231435513142097
    assert(lossTest == lossTruth)
  }

  test("loss() - y = 0, yHat = 0.8") {
    val lossTest = LogisticRegression.loss(y = 0.0, yHat = 0.8)
    val lossTruth = 1.6094379124341005
    assert(lossTest == lossTruth)
  }

  test("gradient() - case 1") {
    val x = List(1.0, 2.0, 3.0)
    val y = 1.0
    val yHat = 0.8
    val gradientTest = LogisticRegression.gradient(x, y, yHat)
    val gradientTruth = List(-0.2, -0.2, -0.4, -0.6)
    gradientTest.zip(gradientTruth).foreach { case (tr, ts) => assert(math.abs(tr - ts) <= Delta) }
  }

  test("inference() - case 1") {
    val weights = List(0.0, -1.0, 1.0, -1.0)
    val x = List(1.0, 1.0, 1.0)
    val inferenceTest = LogisticRegression.inference(x, weights)
    val inferenceTruth = 0.2689414213699951
    assert(inferenceTest == inferenceTruth)
  }


}