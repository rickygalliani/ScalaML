import scala.util.Random

case class WeightVotes(weights: List[List[Double]] = List[List[Double]](),
                       votes: List[Int] = List[Int]()) {

  def getFinalWeights(): List[(List[Double], Double)] = {
    val weightVotes = weights.zip(votes)
    val totalPastVotes = weightVotes.map { case (ws, v) => v }.sum
    weightVotes.map { case (ws, v) => (ws, (1.0 * v) / totalPastVotes) }
  }

}

class VotedPerceptron(var weights: List[(List[Double], Double)] = List[(List[Double], Double)](),
                      val maxEpochs: Int = 25) {

  private val random = new Random

  /**
    Implements the Voted Perceptron learning algorithm:
      - http://curtis.ml.cmu.edu/w/courses/index.php/Voted_Perceptron
  */
  def train(examples: List[Example]): List[(List[Double], Double)] = {
    def trainEpoch(epoch: Int,
                   curWeights: List[Double],
                   pocketWeightVotes: WeightVotes): List[(List[Double], Double)] = {
      if (epoch > maxEpochs) {
        weights = pocketWeightVotes.getFinalWeights()
        return weights
      }
      val randExampleInds = random.shuffle((0 until numExamples).toList)
      var curVotes = 0  // Number of examples this set of weights classifies correctly
      var stillPerfect = true
      while (stillPerfect && curVotes < numExamples) {
        val example = examples(randExampleInds(curVotes))
        val prediction = Perceptron.predict(curWeights, example.X)
        stillPerfect = prediction == example.y
        curVotes = if (stillPerfect) curVotes + 1 else curVotes
      }
      if (stillPerfect) {  // current weights linearly separate the dataset
        weights = List((curWeights, 1.0))
        return weights
      }
      // Update current weights: w = w + x * y where (x, y) is a random misclassified example
      val mistakes = Perceptron.misclassifiedExamples(curWeights, examples)
      val randomMistake = mistakes(random.nextInt(mistakes.length))
      val dw = (List(1.0) ::: randomMistake.X).map(_ * randomMistake.y)
      val newWeights = curWeights.zip(dw).map { case (w, d) => w + d }
      // Add current weights to set of past weights
      val oldWeights = pocketWeightVotes.weights
      val oldVotes = pocketWeightVotes.votes
      val newWeightVotes = WeightVotes(curWeights :: oldWeights, curVotes :: oldVotes)
      trainEpoch(epoch + 1, newWeights, newWeightVotes)
    }
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples passed.")
    val modDim = examples(0).X.length + 1
    val curWeights = List.fill(modDim)(0.0)
    val pocketWeightVotes = WeightVotes()
    weights = List((curWeights, 1.0))
    trainEpoch(epoch = 1, curWeights = curWeights, pocketWeightVotes = pocketWeightVotes)
  }

  def predict(X: List[Double]): Int = VotedPerceptron.predict(weights, X)

  def predictBatch(Xs: List[List[Double]]): List[Int] = Xs.map(predict)

  def linearlySeparates(examples: List[Example]): Boolean = {
    VotedPerceptron.linearlySeparates(weights, examples)
  }

  def misclassifiedExamples(examples: List[Example]): List[Example] = {
    VotedPerceptron.misclassifiedExamples(weights, examples)
  }

}

object VotedPerceptron {

  def predict(weights: List[(List[Double], Double)], x: List[Double]): Int = {
    val score = weights.map { case (w, v) => Perceptron.predict(w, x) * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(weights: List[(List[Double], Double)],
                        examples: List[Example]): Boolean = {
    !examples.exists(ex => VotedPerceptron.predict(weights, ex.X) != ex.y)
  }

  def misclassifiedExamples(weights: List[(List[Double], Double)],
                            examples: List[Example]): List[Example] = {
    examples.filter(ex => VotedPerceptron.predict(weights, ex.X) != ex.y)
  }

}
