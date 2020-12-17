/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.votedperceptron

import example.UnitBinaryClassificationExample
import org.scalameter.api._
import data.TestData

object VotedPerceptronPerfSpec extends Bench.LocalTime {
  
  val MinSize: Int = 10
  val MaxSize: Int = 100
  val StepSize: Int = 5

  val TestSizes: Gen[Int] = Gen.range("numExamples")(MinSize, MaxSize, StepSize)

  val LinearlySeparableExamples: List[UnitBinaryClassificationExample] =
    TestData.generateUnitBinaryClassificationExamples(MaxSize, 0)

    performance of "Linearly Separable Case: VotedPerceptron" in {
    measure method "learn" in {
      using(TestSizes) in { numExamples =>
        val vp = new VotedPerceptron()
        val examples = LinearlySeparableExamples.take(numExamples)
        vp.learn(examples)
      }
    }
  }

}