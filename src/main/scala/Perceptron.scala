import scala.util.Random

class Perceptron(var weights: List[Double] = List[Double]()) {

  private val random = new Random

  def train(examples: List[Example]): List[Double] = {
    def trainRecurse(examples: List[Example],
                     numExamples: Int,
                     curIteration: Int,
                     maxIterations: Int): List[Double] = {
      if (curIteration > maxIterations) return weights
      if (Perceptron.linearlySeparates(this, examples)) return weights
      // Update weights: w = w + x * y where (x, y) is a random misclassified example
      val misclassifiedExs = Perceptron.misclassifiedExamples(this, examples)
      val randomChoice = random.nextInt(misclassifiedExs.length)
      val randomMisclassifiedEx = misclassifiedExs(randomChoice)
      val X = List(1.0) ::: randomMisclassifiedEx.featureVector
      val dw = X.map(x => x * randomMisclassifiedEx.label)
      weights = weights.zip(dw).map { case (w, d) => w + d }
      trainRecurse(examples, numExamples, curIteration + 1, maxIterations)
    }
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples passed.")
    // Training for the first time, initialize weights to 0.0, including bias term as w_0
    val modDim = examples(0).featureVector.length + 1
    weights = List.fill(modDim)(0.0)
    trainRecurse(examples, numExamples, 1, numExamples * 10)
  }

  def predict(example: Example): Int = Perceptron.predict(weights, example)

}

object Perceptron {

  def predict(weights: List[Double], example: Example): Int = {
    val X = List(1.0) ::: example.featureVector
    val xDim = X.length
    val modDim = weights.length
    if (xDim != modDim) {
      throw new IllegalStateException(
        s"Dimension of feature vector (${xDim}) and dimension of model (${modDim}) don't match."
      )
    }
    val score = weights.zip(X).map { case (w, v) => w * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(p: Perceptron, examples: List[Example]): Boolean = {
    !examples.exists(ex => p.predict(ex) != ex.label)
  }

  def misclassifiedExamples(p: Perceptron, examples: List[Example]): List[Example] = {
    examples.filter(ex => p.predict(ex) != ex.label)
  }

}
