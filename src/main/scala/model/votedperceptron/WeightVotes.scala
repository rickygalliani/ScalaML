/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.votedperceptron

case class WeightVotes(weights: List[List[Double]] = List[List[Double]](), votes: List[Int] = List[Int]()) {

  def getFinalWeights: List[(List[Double], Double)] = {
    val weightVotes = weights.zip(votes)
    val totalVotes = weightVotes.map { case (_, v) => v }.sum
    weightVotes.map { case (ws, v) => (ws, (1.0 * v) / totalVotes) }
  }

  def asString: String = {
    s"\n\t - ${weights.zip(votes).map { case (ws, v) => s"$v: ${ws.mkString(",")}" }.mkString("\n\t - ")}"
  }

}
