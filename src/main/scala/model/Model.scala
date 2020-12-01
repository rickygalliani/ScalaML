/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model

import example.Example

trait Model[T <: Example] {

  /**
   * Clients override to process examples (i.e., feature selection, normalization, etc.) before training
   *
   * @param examples list of training examples for preprocessing
   * @return list of training examples to be used in learning algorithm
   */
  def preprocess(examples: List[Example]): List[T]

  /**
   * Clients override to implement learning algorithm and updatie internal state with learned parameters
   *
   * @param examples list of processed training examples
   */
  protected def learn(examples: List[T]): Unit

  /**
   * Clients override to implement model inference
   *
   * @param X a feature vector
   * @return a prediction
   */
  def predict(X: List[Double]): Int

  /**
   * Public interface for model training
   *
   * @param examples a list of examples, unprocessed
   */
  final def train(examples: List[Example]): Unit = {
    learn(preprocess(examples))
  }

  /**
   * Public interface for batch prediction
   *
   * @param Xs a list of feature vectors
   * @return a list of predictions
   */
  final def predictBatch(Xs: List[List[Double]]): List[Int] = Xs.map(predict)

}
