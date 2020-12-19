/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.logisticregression

import data.TestData
import example.BinaryClassificationExample
import org.scalameter.api._

object LogisticRegressionPerfSpec extends Bench.LocalTime {

  val NumEpochs = 100

  val MinSize: Int = 10
  val MaxSize: Int = 100
  val StepSize: Int = 5
  val NumOutliers: Int = MaxSize / MinSize

  val TestSizes: Gen[Int] = Gen.range("numExamples")(MinSize, MaxSize, StepSize)

  val Examples: List[BinaryClassificationExample] = TestData.generateBinaryClassificationExamples(MaxSize, 0)

  performance of "model.classification.logisticregression.LogisticRegression" in {
    measure method "learn" in {
      using(TestSizes) in { numExamples =>
        val lr = new LogisticRegression(epochs = NumEpochs, verbose = false)
        lr.learn(Examples.take(numExamples))
      }
    }
  }

}