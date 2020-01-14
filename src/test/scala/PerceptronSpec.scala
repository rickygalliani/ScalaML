import org.scalatest.FunSuite
  
class PerceptronSpec extends FunSuite {

  test("linearlySeparates() - positive case 1: 0 weights") {
    val p = new Perceptron()
    p.weights = List(0.0, 0.0, 0.0)
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(Perceptron.linearlySeparates(p, examples) == false)
  }

  test("linearlySeparates() - negative case 1: 2 points, 2 classes") {
    val p = new Perceptron()
    p.weights = List(0.0, 0.0, 2.0)
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(Perceptron.linearlySeparates(p, examples) == true)
  }

  test("misclassifiedExamples() - case 1: 2 points, 1 misclassified") {
    val p = new Perceptron()
    p.weights = List(0.0, 0.0, 0.0)
    val badEx = new Example(List(1.0, -1.0), -1)
    val examples = List(new Example(List(1.0, 1.0), 1), badEx)
    assert(Perceptron.misclassifiedExamples(p, examples) == List(badEx))
  }

  test("train() - linear separable case 1: 2 points, 2 classes") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    val weights = p.train(examples)
    assert(Perceptron.linearlySeparates(p, examples) == true)
    assert(Perceptron.misclassifiedExamples(p, examples).isEmpty)
  }

  test("train() - linear separable case 2: 45 degree line") {
    val p = new Perceptron()
    // Positive examples above 45 degree line, negative examples below
    val examples = (-10 to 10).toList.flatMap { n =>
      val exPos = new Example(List(n, n + 1), 1)
      val exNeg = new Example(List(n, n - 1), -1)
      List(exPos, exNeg)
    }
    val weights = p.train(examples)
    assert(Perceptron.linearlySeparates(p, examples) == true)
    assert(Perceptron.misclassifiedExamples(p, examples).isEmpty)
  }

  test("train(): linear separable case 3: 3 dimensions") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), -1)
    )
    val weights = p.train(examples)
    assert(Perceptron.linearlySeparates(p, examples) == true)
    assert(Perceptron.misclassifiedExamples(p, examples).isEmpty)
  }

  test("train(): linear separable case 4: only 1 class") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), 1)
    )
    val weights = p.train(examples)
    assert(Perceptron.linearlySeparates(p, examples) == true)
    assert(Perceptron.misclassifiedExamples(p, examples).isEmpty)
  }
}