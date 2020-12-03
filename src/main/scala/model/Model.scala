/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model

import data.normalize.Normalizer
import example.Example

abstract class Model[T <: Example](val normalizer: Option[Normalizer] = None) {

  /**
   * Private interface for feature normalization used before training and prediction
   *
   * @param Xs list of feature vectors
   * @return list of feature vectors, normalized
   */
  protected def normalize(Xs: List[List[Double]]): List[List[Double]] = {
    normalizer match {
      case Some(n) =>
        // Change feature list representation from per-user (row-wise Xs) to per-feature (column-wise features)
        Xs.transpose.zipWithIndex.map { case (xs, index) => n.normalize(index, xs) }.transpose
      case _ => Xs // no feature normalization
    }
  }

  /**
   * Clients implement to change type of training examples
   *
   * @param examples list of training examples passed from client
   * @return list of training examples of type expected by learning algorithm
   */
  def cast(examples: List[Example]): List[T]

  /**
   * Clients implement to process examples (i.e., feature selection, normalization, etc.) before training
   *
   * @param examples list of training examples for preprocessing
   * @return list of training examples to be used in learning algorithm
   */
  final def preprocess(examples: List[Example]): List[T] = {
    val normalizedXs = normalize(examples.map(_.X))
    val ys = examples.map(_.y)
    val exs: List[Example] = normalizedXs.zip(ys).map { case (x, y) => new Example(x, y) }
    cast(exs)
  }

  /**
   * Clients implement to implement learning algorithm and updatie internal state with learned parameters
   *
   * @param examples list of processed training examples
   */
  protected def learn(examples: List[T]): Unit

  /**
   * Clients implement to define model inference on a single feature vector
   *
   * @param X a feature vector
   * @return a prediction
   */
  def inference(X: List[Double]): Double

  /**
   * Public interface for model training
   *
   * @param examples a list of examples, unprocessed
   */
  final def train(examples: List[Example]): Unit = learn(preprocess(examples))

  /**
   * Public interface for model inference on a single feature vector
   *
   * @param X a feature vector
   * @return a prediction
   */
  final def predict(X: List[Double]): Double = inference(normalize(List(X)).head)

  /**
   * Public interface for batch prediction
   *
   * @param Xs a list of feature vectors
   * @return a list of predictions
   */
  final def predictBatch(Xs: List[List[Double]]): List[Double] = normalize(Xs).map(inference)

}
