import org.scalatest.FunSuite
  
class VotedPerceptronSpec extends FunSuite {

  test("linearlySeparates() - positive case 1: 0 weights") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(0.0, 0.0, 0.0), 1.0))
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(VotedPerceptron.linearlySeparates(vp, examples) == false)
  }

  test("linearlySeparates() - negative case 1: 2 points, 2 classes") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(0.0, 0.0, 2.0), 1.0))
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(VotedPerceptron.linearlySeparates(vp, examples) == true)
  }

  test("misclassifiedExamples() - case 1: 2 points, 1 misclassified") {
    val vp = new VotedPerceptron()
    vp.weights = List((List(0.0, 0.0, 0.0), 1.0))
    val badEx = new Example(List(1.0, -1.0), -1)
    val examples = List(new Example(List(1.0, 1.0), 1), badEx)
    assert(VotedPerceptron.misclassifiedExamples(vp, examples) == List(badEx))
  }

  test("train() - linear separable case 1: 2 points, 2 classes") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    val weights = vp.train(examples)
    assert(VotedPerceptron.linearlySeparates(vp, examples) == true)
    assert(VotedPerceptron.misclassifiedExamples(vp, examples).isEmpty)
  }

  test("train() - linear separable case 2: 45 degree line") {
    val vp = new VotedPerceptron()
    // Positive examples above 45 degree line, negative examples below
    val examples = (-10 to 10).toList.flatMap { n =>
      val exPos = new Example(List(n, n + 1), 1)
      val exNeg = new Example(List(n, n - 1), -1)
      List(exPos, exNeg)
    }
    val weights = vp.train(examples)
    assert(VotedPerceptron.linearlySeparates(vp, examples) == true)
    assert(VotedPerceptron.misclassifiedExamples(vp, examples).isEmpty)
  }

  test("train(): linear separable case 3: 3 dimensions") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), -1)
    )
    val weights = vp.train(examples)
    assert(VotedPerceptron.linearlySeparates(vp, examples) == true)
    assert(VotedPerceptron.misclassifiedExamples(vp, examples).isEmpty)
  }

  test("train(): linear separable case 4: only 1 class") {
    val vp = new VotedPerceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), 1)
    )
    val weights = vp.train(examples)
    assert(VotedPerceptron.linearlySeparates(vp, examples) == true)
    assert(VotedPerceptron.misclassifiedExamples(vp, examples).isEmpty)
  }
}