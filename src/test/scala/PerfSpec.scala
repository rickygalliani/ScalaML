import org.scalameter.api._

object RangeBenchmark extends Bench.LocalTime {
  
  val MinSize = 10
  val MaxSize = 100
  val StepSize = 5
  val NumOutliers = MaxSize / MinSize

  val testSizes = Gen.range("numExamples")(MinSize, MaxSize, StepSize)

  val linearlySeparableExamples = TestUtility.generateBinaryExamples(MaxSize, 0)
  val linearlyInseparableExamples = TestUtility.generateBinaryExamples(MaxSize, NumOutliers)

  performance of "Linearly Inseparable Case: Perceptron" in {
    measure method "train" in {
      using(testSizes) in { numExamples =>
        val p = new Perceptron()
        val examples = linearlyInseparableExamples.take(numExamples)
        p.train(examples)
      }
    }
  }

  performance of "Linearly Separable Case: Perceptron" in {
    measure method "train" in {
      using(testSizes) in { numExamples =>
        val p = new Perceptron()
        val examples = linearlySeparableExamples.take(numExamples)
        p.train(examples)
      }
    }
  }

  performance of "Linearly Inseparable Case: VotedPerceptron" in {
    measure method "train" in {
      using(testSizes) in { numExamples =>
        val vp = new VotedPerceptron()
        val examples = linearlyInseparableExamples.take(numExamples)
        vp.train(examples)
      }
    }
  }

    performance of "Linearly Separable Case: VotedPerceptron" in {
    measure method "train" in {
      using(testSizes) in { numExamples =>
        val vp = new VotedPerceptron()
        val examples = linearlySeparableExamples.take(numExamples)
        vp.train(examples)
      }
    }
  }

}