/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package votedperceptron

import example.BinaryClassificationExample
import org.scalameter.api._
import data.TestData

object VotedPerceptronPerfSpec extends Bench.LocalTime {
  
  val MinSize: Int = 10
  val MaxSize: Int = 100
  val StepSize: Int = 5
  val NumOutliers: Int = MaxSize / MinSize

  val TestSizes: Gen[Int] = Gen.range("numExamples")(MinSize, MaxSize, StepSize)

  val LinearlySeparableExamples: List[BinaryClassificationExample] =
    TestData.generateBinaryClassificationExamples(MaxSize, 0)
  val LinearlyInseparableExamples: List[BinaryClassificationExample] =
    TestData.generateBinaryClassificationExamples(MaxSize, NumOutliers)

  performance of "Linearly Inseparable Case: VotedPerceptron" in {
    measure method "train" in {
      using(TestSizes) in { numExamples =>
        val vp = new VotedPerceptron()
        val examples = LinearlyInseparableExamples.take(numExamples)
        vp.train(examples)
      }
    }
  }

    performance of "Linearly Separable Case: VotedPerceptron" in {
    measure method "train" in {
      using(TestSizes) in { numExamples =>
        val vp = new VotedPerceptron()
        val examples = LinearlySeparableExamples.take(numExamples)
        vp.train(examples)
      }
    }
  }

}