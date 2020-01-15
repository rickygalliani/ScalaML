import org.scalatest.FunSuite
  
class PerceptronSpec extends FunSuite {

  test("linearlySeparates() - positive case 1: 0 weights") {
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(Perceptron.linearlySeparates(List(0.0, 0.0, 0.0), examples) == false)
  }

  test("linearlySeparates() - negative case 1: 2 points, 2 classes") {
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    assert(Perceptron.linearlySeparates(List(0.0, 0.0, 2.0), examples) == true)
  }

  test("misclassifiedExamples() - case 1: 2 points, 1 misclassified") {
    val badEx = new Example(List(1.0, -1.0), -1)
    val examples = List(new Example(List(1.0, 1.0), 1), badEx)
    assert(Perceptron.misclassifiedExamples(List(0.0, 0.0, 0.0), examples) == List(badEx))
  }

  test("train() - linear separable case 1: 2 points, 2 classes") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0), 1),
      new Example(List(1.0, -1.0), -1)
    )
    p.train(examples)
    assert(p.linearlySeparates(examples) == true)
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("train() - linear separable case 2: 45 degree line") {
    val p = new Perceptron()
    // Positive examples above 45 degree line, negative examples below
    val examples = (-10 to 10).toList.flatMap { n =>
      val exPos = new Example(List(n, n + 1), 1)
      val exNeg = new Example(List(n, n - 1), -1)
      List(exPos, exNeg)
    }
    p.train(examples)
    assert(p.linearlySeparates(examples) == true)
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("train(): linear separable case 3: 3 dimensions") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), -1)
    )
    p.train(examples)
    assert(p.linearlySeparates(examples) == true)
    assert(p.misclassifiedExamples(examples).isEmpty)
  }

  test("train(): linear separable case 4: only 1 class") {
    val p = new Perceptron()
    val examples = List(
      new Example(List(1.0, 1.0, 1.0), 1),
      new Example(List(-1.0, -1.0, -1.0), 1)
    )
    p.train(examples)
    assert(p.linearlySeparates(examples) == true)
    assert(p.misclassifiedExamples(examples).isEmpty)
  }
}