import scala.util.Random

case class WeightVotes(weights: List[List[Double]] = List[List[Double]](),
                       votes: List[Int] = List[Int]()) {

  def computeWeights(): List[(List[Double], Double)] = {
    val weightVotes = weights.zip(votes)
    val totalPastVotes = weightVotes.map { case (ws, v) => v }.sum
    weightVotes.map { case (ws, v) => (ws, (1.0 * v) / totalPastVotes) }
  }

}

class VotedPerceptron(var weights: List[(List[Double], Double)] = List[(List[Double], Double)]()) {

  private val random = new Random

  /**
    Implements the Voted Perceptron learning algorithm:
      - http://curtis.ml.cmu.edu/w/courses/index.php/Voted_Perceptron
  */
  def train(examples: List[Example]): List[(List[Double], Double)] = {
    def trainRecurse(examples: List[Example],
                     numExamples: Int,
                     curIteration: Int,
                     maxIterations: Int,
                     curWeights: List[Double],
                     curWeightVotes: WeightVotes): List[(List[Double], Double)] = {
      if (curIteration > maxIterations) {
        weights = curWeightVotes.computeWeights()
        return weights
      }
      val randExampleInds = random.shuffle((0 until numExamples).toList)
      var curVotes = 0  // Number of examples this set of weights classifies correctly
      var stillPerfect = true 
      while (stillPerfect) {
        val example = examples(curVotes)
        val prediction = Perceptron.predict(curWeights, example)
        stillPerfect = prediction == example.label
        curVotes = if (stillPerfect) curVotes + 1 else curVotes
      }
      if (stillPerfect) {  // current weights linearly separate the dataset
        weights = List((curWeights, 1.0))
        return weights
      }
      // Update current weights: w = w + x * y where (x, y) is a random misclassified example
      val misclassifiedExs = VotedPerceptron.misclassifiedExamples(this, examples)
      val randomChoice = random.nextInt(misclassifiedExs.length)
      val randomMisclassifiedEx = misclassifiedExs(randomChoice)
      val X = List(1.0) ::: randomMisclassifiedEx.featureVector
      val dw = X.map(x => x * randomMisclassifiedEx.label)
      val newCurWeights = curWeights.zip(dw).map { case (w, d) => w + d }
      // Add current weights to set of past weights
      val oldWeights = curWeightVotes.weights
      val oldVotes = curWeightVotes.votes
      val newWeightVotes = WeightVotes(curWeights :: oldWeights, curVotes :: oldVotes)
      trainRecurse(examples,
                   numExamples,
                   curIteration + 1,
                   maxIterations,
                   newCurWeights,
                   newWeightVotes)
    }
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples passed.")
    val modDim = examples(0).featureVector.length + 1
    val curWeights = List.fill(modDim)(0.0)
    val curWeightVotes = WeightVotes()
    weights = List((curWeights, 1.0))
    trainRecurse(examples, numExamples, 1, numExamples * 10, curWeights, curWeightVotes)
  }

  def predict(example: Example): Int = VotedPerceptron.predict(weights, example)

}

object VotedPerceptron {

  def predict(weights: List[(List[Double], Double)], example: Example): Int = {
    val score = weights.map { case (w, v) => Perceptron.predict(w, example) * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(vp: VotedPerceptron, examples: List[Example]): Boolean = {
    !examples.exists(ex => vp.predict(ex) != ex.label)
  }

  def misclassifiedExamples(vp: VotedPerceptron, examples: List[Example]): List[Example] = {
    examples.filter(ex => vp.predict(ex) != ex.label)
  }

}
