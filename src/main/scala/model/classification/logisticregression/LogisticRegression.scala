/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.logisticregression

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import op.LinAlg.{dot, scalarMultiply}
import org.apache.logging.log4j.Level

import scala.util.Random

class LogisticRegression(var weights: List[Double] = List[Double](),
                         val epochs: Int = Epochs,
                         val learningRate: Double = LearningRate,
                         override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer()),
                         override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val lr: Level = Level.forName("logisticregression", LogLevelSeed)

  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val numExamples = examples.size
    weights = List.fill(examples.head.X.length + 1)(0.0)
    (1 to epochs).foreach { epoch =>
      var cost = 0.0
      var grad = List.fill(examples.head.X.length + 1)(0.0)
      examples.foreach { ex =>
        val yHat = inference(ex.X)
        cost += LogisticRegression.loss(ex.y, yHat) // Compute loss for each training example
        // Compute contribution of each example to gradient
        grad = LogisticRegression.gradient(ex.X, ex.y, yHat).zipWithIndex.map { case (g, index) => grad(index) + g }
      }
      cost /= numExamples
      grad = grad.map(_ / numExamples)
      weights = weights.zip(grad).map { case (w, dw) => w - learningRate * dw }
      if (epoch % LogFrequency == 0 && verbose) { logger(lr, s"Epoch: $epoch, Cost: $cost") }
    }
  }

  override def inference(x: List[Double]): Double = LogisticRegression.inference(x, this.weights)

}

object LogisticRegression {

  def loss(y: Double, yHat: Double): Double = {
    if (y == 0.0) { -1.0 * math.log(1 - yHat) }
    else { -1.0 * math.log(yHat) }
  }

  def gradient(x: List[Double], y: Double, yHat: Double): List[Double] = scalarMultiply(yHat - y, List(1.0) ::: x)

  def inference(x: List[Double], weights: List[Double]): Double = 1.0 / (1 + math.exp(-dot(weights, List(1.0) ::: x)))

}
