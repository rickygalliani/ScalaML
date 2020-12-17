/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.perceptron

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.{Example, UnitBinaryClassificationExample}
import model.UnitBinaryClassificationModel
import org.apache.logging.log4j.Level

import scala.annotation.tailrec
import scala.util.Random

class Perceptron(var weights: List[Double] = List[Double](),
                 val maxEpochs: Int = MaxEpochs,
                 override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer()),
                 override val verbose: Boolean = false) extends UnitBinaryClassificationModel {

  private val random = new Random
  val pn: Level = Level.forName("perceptron", LogLevelSeed)

  override def learn(examples: List[UnitBinaryClassificationExample]): Unit = {

    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val numExamples = examples.length
    if (numExamples == 0) throw new IllegalStateException("No training examples.")

    // Updates weights attribute in place
    @tailrec
    def trainEpoch(epoch: Int, pocketWeights: List[Double], pocketMistakes: Int): Unit = {
      if (epoch < maxEpochs) {
        val mistakes = misclassifiedExamples(examples)
        val numMistakes = mistakes.length
        if (numMistakes > 0) {
          // Save best weights and number of mistakes "in pocket"
          val (newPocketWeights, newPocketMistakes) = {
            if (numMistakes <= pocketMistakes) { (weights, numMistakes) }
            else { (pocketWeights, pocketMistakes) }
          }
          val randomMistake = mistakes(random.nextInt(numMistakes))
          val dw = (List(1.0) ::: randomMistake.X).map(d => d * randomMistake.y)
          // Update weights: w = w + x * y where (x, y) is a random misclassified example
          weights = weights.zip(dw).map { case (w, d) => w + d }
          if (epoch % LogFrequency == 0 && verbose) { logger(pn, s"Epoch: $epoch, Mistakes: $numMistakes") }
          trainEpoch(epoch + 1, newPocketWeights, newPocketMistakes)
        }
      }
    }

    // Training for the first time, initialize weights to 0.0, including bias term as w_0
    weights = List.fill(examples.head.X.length + 1)(0.0)
    trainEpoch(epoch = 1, pocketWeights = weights, pocketMistakes = numExamples)
  }

  override def inference(X: List[Double]): Double = Perceptron.inference(weights, X)

  def linearlySeparates(examples: List[UnitBinaryClassificationExample]): Boolean = {
    Perceptron.linearlySeparates(weights, examples)
  }

  def misclassifiedExamples(examples: List[UnitBinaryClassificationExample]): List[Example] = {
    Perceptron.misclassifiedExamples(weights, examples)
  }

}

object Perceptron {

  def inference(weights: List[Double], x: List[Double]): Double = {
    val X = List(1.0) ::: x
    val xDim = X.length
    val modDim = weights.length
    if (xDim != modDim) {
      throw new IllegalStateException(s"Dimension of feature vector ($xDim) and model ($modDim) don't match.")
    }
    val score = weights.zip(X).map { case (w, v) => w * v }.sum
    if (score >= 0) 1 else -1
  }

  def linearlySeparates(weights: List[Double],
                        examples: List[UnitBinaryClassificationExample]): Boolean = {
    !examples.exists(ex => Perceptron.inference(weights, ex.X) != ex.y)
  }

  def misclassifiedExamples(weights: List[Double], examples: List[UnitBinaryClassificationExample]):
  List[UnitBinaryClassificationExample] = {
    examples.filter(ex => Perceptron.inference(weights, ex.X) != ex.y)
  }

}
