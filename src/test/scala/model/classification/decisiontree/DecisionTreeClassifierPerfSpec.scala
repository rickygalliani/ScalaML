/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import data.TestData
import example.BinaryClassificationExample
import org.scalameter.api._

class DecisionTreeClassifierPerfSpec extends Bench.LocalTime {

  val MaxDepth = 3
  val MinSize: Int = 10
  val MaxSize: Int = 100
  val StepSize: Int = 5
  val NumOutliers: Int = MaxSize / MinSize

  val TestSizes: Gen[Int] = Gen.range("numExamples")(MinSize, MaxSize, StepSize)

  val Examples: List[BinaryClassificationExample] = TestData.generateBinaryClassificationExamples(MaxSize, 0)

  performance of "model.classification.decisiontree.DecisionTreeClassifier" in {
    measure method "learn" in {
      using(TestSizes) in { numExamples =>
        val dtc = new DecisionTreeClassifier(maxDepth = 3, verbose = false)
        dtc.learn(Examples.take(numExamples))
      }
    }
  }

}
