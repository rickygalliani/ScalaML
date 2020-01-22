import org.scalatest.FunSuite
  
class VotedPerceptronSpec extends FunSuite {

  test("linearlySeparates() - positive case 1: 0 weights") {
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(VotedPerceptron.linearlySeparates(List((List(0.0, 0.0, 0.0), 1.0)), examples) == false)
  }

  test("linearlySeparates() - negative case 1: 2 points, 2 classes") {
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(VotedPerceptron.linearlySeparates(List((List(0.0, 0.0, 2.0), 1.0)), examples) == true)
  }

  test("misclassifiedExamples() - case 1: 2 points, 1 misclassified") {
    val badEx = new Example(List(1.0, -1.0), -1)
    val examples = List(new Example(List(1.0, 1.0), 1), badEx)
    val weights = List((List(0.0, 0.0, 0.0), 1.0))
    assert(VotedPerceptron.misclassifiedExamples(weights, examples) == List(badEx))
  }

  test("train() - linear separable case 1: 2 points, 2 classes") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    vp.train(examples)
    assert(VotedPerceptron.linearlySeparates(vp.weights, examples) == true)
    assert(VotedPerceptron.misclassifiedExamples(vp.weights, examples).isEmpty)
  }

  test("train() - linear separable case 2: 45 degree line") {
    val vp = new VotedPerceptron()
    // Positive examples above 45 degree line, negative examples below
    val examples = (-10 to 10).toList.flatMap { n =>
      val exPos = new Example(List(n, n + 1), 1)
      val exNeg = new Example(List(n, n - 1), -1)
      List(exPos, exNeg)
    }
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == true)
    assert(vp.misclassifiedExamples(examples).isEmpty)
  }

  test("train() - linear separable case 3: 3 dimensions") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), -1)
    )
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == true)
    assert(vp.misclassifiedExamples(examples).isEmpty)
  }

  test("train() - linear separable case 4: only 1 class") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), 1)
    )
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == true)
    assert(vp.misclassifiedExamples(examples).isEmpty)
  }

  test("train() - linear inseparable case 1: 2 same points, 2 classes") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(1.0, 1.0, 1.0), -1)
    )
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == false)
    assert(vp.misclassifiedExamples(examples).length == 1)
  }

  test("train() - linear inseparable case 1: 1 outlier") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), -1),
      new Example(List(-1.0, -1.0, -1.0), -1),
      new Example(List(-1.0, -1.0, -1.0), -1),
      new Example(List(-1.0, -1.0, -1.0), 1)  // outlier
    )
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == false)
    assert(vp.misclassifiedExamples(examples).length == 1)
  }

  test("train() - linearly inseparable case 2: several outliers") {
    val TestSize = 50
    val NumOutliers = 3

    val vp = new VotedPerceptron()
    val examples = TestUtility.generateBinaryExamples(TestSize, NumOutliers)
    vp.train(examples)
    assert(vp.linearlySeparates(examples) == false)
    assert(vp.misclassifiedExamples(examples).length == NumOutliers * 2)
  }

  test("predict() - case 1: 3 dimensions, positive label") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 1.0))
    val prediction = vp.predict(List(1, 1))
    assert(prediction == 1)
  }

  test("predict() - case 2: 3 dimensions, negative label") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(-1.0, -2.0, -3.0), 1.0))
    val prediction = vp.predict(List(1, 1))
    assert(prediction == -1)
  }

  test("predict() - case 3: 3 dimensions, 2 vectors") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 0.5), (List(2.0, 4.0, 6.0), 0.5))
    val prediction = vp.predict(List(1, 1))
    assert(prediction == 1)
  } 

  test("predictBatch() - case 1: 2 dimensions 1 vector") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 1.0))
    val predictions = vp.predictBatch(List(List(1, 1), List(-1, -1)))
    assert(predictions == List(1, -1))
  }

  test("predictBatch() - case 2: 3 dimensions 2 vectors") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(1.0, 2.0, 3.0), 0.5), (List(2.0, 4.0, 6.0), 0.5))
    val predictions = vp.predictBatch(List(List(1, 1), List(-1, -1)))
    assert(predictions == List(1, -1))
  }

}