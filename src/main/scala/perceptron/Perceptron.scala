/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package perceptron

import example.Example

import scala.annotation.tailrec
import scala.util.Random

class Perceptron(var weights: List[Double] = List[Double](), val maxEpochs: Int = MaxEpochs) {

  private val random = new Random

  def train(examples: List[Example]): List[Double] = {
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples.")

    @tailrec
    def trainEpoch(epoch: Int, pocketWeights: List[Double], pocketMistakes: Int): List[Double] = {
      if (epoch >= maxEpochs) {
        pocketWeights
      }
      else {
        val mistakes = misclassifiedExamples(examples)
        val numMistakes = mistakes.length
        if (numMistakes == 0) {
          weights
        }
        else {
          // Save best weights and number of mistakes "in pocket"
          val (newPocketWeights, newPocketMistakes) = {
            if (numMistakes <= pocketMistakes) {
              (weights, numMistakes)
            }
            else {
              (pocketWeights, pocketMistakes)
            }
          }
          val randomMistake = mistakes(random.nextInt(numMistakes))
          val dw = (List(1.0) ::: randomMistake.X).map(d => d * randomMistake.y)
          // Update weights: w = w + x * y where (x, y) is a random misclassified example
          weights = weights.zip(dw).map { case (w, d) => w + d }
          trainEpoch(epoch + 1, newPocketWeights, newPocketMistakes)
        }
      }
    }

    // Training for the first time, initialize weights to 0.0, including bias term as w_0
    weights = List.fill(examples.head.X.length + 1)(0.0)
    trainEpoch(epoch = 1, pocketWeights = weights, pocketMistakes = numExamples)
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

  def predict(weights: List[Double], x: List[Double]): Int = {
    val X = List(1.0) ::: x
    val xDim = X.length
    val modDim = weights.length
    if (xDim != modDim) {
      throw new IllegalStateException(
        s"Dimension of feature vector ($xDim) and model ($modDim) don't match."
      )
    }
    val score = weights.zip(X).map { case (w, v) => w * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(weights: List[Double], examples: List[Example]): Boolean = {
    !examples.exists(ex => Perceptron.predict(weights, ex.X) != ex.y)
  }

  def misclassifiedExamples(weights: List[Double], examples: List[Example]): List[Example] = {
    examples.filter(ex => Perceptron.predict(weights, ex.X) != ex.y)
  }

}
