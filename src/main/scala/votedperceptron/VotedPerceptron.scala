/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package votedperceptron

import example.{BinaryClassificationExample, PosNegBinaryClassificationExample, binaryToPosNeg}
import perceptron.Perceptron

import scala.annotation.tailrec
import scala.util.Random

class VotedPerceptron(var weights: List[(List[Double], Double)] = List[(List[Double], Double)](),
                      val maxEpochs: Int = MaxEpochs) {

  private val random = new Random
  random.setSeed(TrainSeed)

  /**
   * Implements the Voted perceptron.Perceptron learning algorithm:
   * - http://curtis.ml.cmu.edu/w/courses/index.php/Voted_Perceptron
   */
  def train(examples: List[BinaryClassificationExample]): Unit = {
    val perceptronExamples = examples.map(e => PosNegBinaryClassificationExample(e))
    random.shuffle(perceptronExamples)
    val numExamples = perceptronExamples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples passed.")

    @tailrec
    def trainEpoch(epoch: Int, pocketWeightVotes: WeightVotes): Unit = {
      if (epoch >= maxEpochs) { weights = pocketWeightVotes.getFinalWeights }
      else {
        val randExampleInds = random.shuffle((0 until numExamples).toList)
        val curWeights = pocketWeightVotes.weights.head
        var curVotes = 0 // Number of examples this set of weights classifies correctly
        var stillPerfect = true
        while (stillPerfect && curVotes < numExamples) {
          val example = perceptronExamples(randExampleInds(curVotes))
          val prediction = Perceptron.predict(curWeights, example.X)
          stillPerfect = prediction == example.y
          curVotes = if (stillPerfect) curVotes + 1 else curVotes
        }
        // current weights linearly separate the dataset
        if (stillPerfect) { weights = WeightVotes(List(curWeights), List[Int](1)).getFinalWeights }
        else {
          // Update current weights: w = w + x * y where (x, y) is a random misclassified example
          val mistakes = Perceptron.misclassifiedExamples(curWeights, perceptronExamples)
          val numMistakes = mistakes.size
          if (numMistakes == 0) {
            weights = pocketWeightVotes.getFinalWeights
          }
          else {
            val randomMistake = mistakes(random.nextInt(numMistakes))
            val dw = (List(1.0) ::: randomMistake.X).map(_ * randomMistake.y)
            val newWeights = curWeights.zip(dw).map { case (w, d) => w + d }
            // Add current weights to set of past weights
            val newWeightVotes = {
              if (curVotes == 0) {
                pocketWeightVotes
              }
              else {
                val newPocketWeights = newWeights :: curWeights :: pocketWeightVotes.weights.tail
                val newPocketVotes = 0 :: curVotes :: pocketWeightVotes.votes.tail
                WeightVotes(newPocketWeights, newPocketVotes)
              }
            }
            trainEpoch(epoch + 1, newWeightVotes)
          }
        }
      }
    }

    // Training for the first time, initialize weights to 0.0 as only "voting" weights
    val modDim = perceptronExamples.head.X.length + 1
    val pocketWeightVotes = WeightVotes(List(List.fill(modDim)(0.0)), List(1))
    trainEpoch(epoch = 1, pocketWeightVotes = pocketWeightVotes)
  }

  def predict(X: List[Double]): Int = VotedPerceptron.predict(weights, X)

  def predictBatch(Xs: List[List[Double]]): List[Int] = Xs.map(predict)

}

object VotedPerceptron {

  def predict(weights: List[(List[Double], Double)], x: List[Double]): Int = {
    val score = weights.map { case (w, v) => binaryToPosNeg(Perceptron.predict(w, x)) * v }.sum
    if (score >= 0) 1 else 0
  }

}