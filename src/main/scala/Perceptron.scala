import scala.util.Random

class Perceptron(var weights: List[Double] = List[Double]()) {

  private val random = new Random

  def train(examples: List[Example]): List[Double] = {
    def trainEpoch(examples: List[Example],
                   numExamples: Int,
                   curEpoch: Int,
                   maxEpochs: Int,
                   bestWeightsSoFar: List[Double],
                   fewestMistakesSoFar: Int): List[Double] = {
      if (curEpoch > maxEpochs) {
        weights = bestWeightsSoFar
        return weights
      }
      val mistakes = misclassifiedExamples(examples)
      val numMistakes = mistakes.length
      if (numMistakes == 0) return weights
      // Prepare "pocket" data for next training step
      val (newWeights, newMistakes) = {
        if (numMistakes <= fewestMistakesSoFar) (weights, numMistakes)
        else (bestWeightsSoFar, fewestMistakesSoFar)
      }
      val randomMistake = mistakes(random.nextInt(numMistakes))
      val dw = (List(1.0) ::: randomMistake.X).map(_ * randomMistake.y)
      // Update weights: w = w + x * y where (x, y) is a random misclassified example
      weights = weights.zip(dw).map { case (w, d) => w + d }
      trainEpoch(examples, numExamples, curEpoch + 1, maxEpochs, newWeights, newMistakes)
    }
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples passed.")
    // Training for the first time, initialize weights to 0.0, including bias term as w_0
    weights = List.fill(examples(0).X.length + 1)(0.0)
    trainEpoch(examples, numExamples, 1, numExamples * 10, weights, numExamples)
  }

  def predict(X: List[Double]): Int = Perceptron.predict(weights, X)

  def predictBatch(Xs: List[List[Double]]): List[Int] = Xs.map(predict)

  def linearlySeparates(examples: List[Example]): Boolean = {
    Perceptron.linearlySeparates(weights, examples)
  }

  def misclassifiedExamples(examples: List[Example]): List[Example] = {
    Perceptron.misclassifiedExamples(weights, examples)
  }

}

object Perceptron {

  def predict(weights: List[Double], X: List[Double]): Int = {
    val X_ = List(1.0) ::: X
    val xDim = X_.length
    val modDim = weights.length
    if (xDim != modDim) {
      throw new IllegalStateException(
        s"Dimension of feature vector (${xDim}) and dimension of model (${modDim}) don't match."
      )
    }
    val score = weights.zip(X_).map { case (w, v) => w * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(weights: List[Double], examples: List[Example]): Boolean = {
    !examples.exists(ex => Perceptron.predict(weights, ex.X) != ex.y)
  }

  def misclassifiedExamples(weights: List[Double], examples: List[Example]): List[Example] = {
    examples.filter(ex => Perceptron.predict(weights, ex.X) != ex.y)
  }

}
