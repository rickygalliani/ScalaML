import scala.util.Random

class Perceptron(var weights: List[Double] = List[Double]()) {

    private val random = new Random

    def train(examples: List[Example],
              curIteration: Int = 0,
              maxIterations: Int = 1000): List[Double] = {
        if (weights.isEmpty) {
            // Training for the first time, initialize weights to 0.0,
            // including a bias term as w_0
            val modDim = examples(0).featureVector.length + 1
            weights = List.fill(modDim)(0.0)
        }
        val perfectClassification = Perceptron.linearlySeparates(this, examples)
        if (perfectClassification || curIteration > maxIterations) {
            return weights
        }
        // Update weights according to: w(t + 1) = w(t) + y(t)x(t)
        // where (x(t), y(t)) is a currently misclassified example
        val misclassifiedExs = Perceptron.misclassifiedExamples(this, examples)
        val randomChoice = random.nextInt(misclassifiedExs.length)
        val randomMisclassifiedEx = misclassifiedExs(randomChoice)
        val X = List(1.0) ::: randomMisclassifiedEx.featureVector
        val dw = X.map(x => x * randomMisclassifiedEx.label)
        weights = weights.zip(dw).map { case (w, d) => w + d }
        train(examples, curIteration + 1)
    }

    def predict(example: Example): Int = {
        val X = List(1.0) ::: example.featureVector
        val xDim = X.length
        val modDim = weights.length
        if (xDim != modDim) {
            throw new IllegalStateException(s"""Dimension of feature vector
                |(${xDim}) and dimension of model (${modDim}) don't match."""
                    .stripMargin
                    .replaceAll("\n", " ")
            )
        }
        val score = weights.zip(X).map { case (w, v) => w * v }.sum
        if (score >= 0) 1 else -1
    }
}

object Perceptron {
    def linearlySeparates(p: Perceptron, examples: List[Example]): Boolean = {
        !examples.exists(ex => p.predict(ex) != ex.label)
    }

    def misclassifiedExamples(p: Perceptron,
                              examples: List[Example]): List[Example] = {
        examples.filter(ex => p.predict(ex) != ex.label)
    }
}
