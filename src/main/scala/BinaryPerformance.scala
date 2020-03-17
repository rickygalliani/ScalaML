object BinaryPerformance {

  def computePrecision(predictions: List[Int], labels: List[Int]): Double = {
    val numPredictions = predictions.length
    val numLabels = labels.length
    if (numPredictions != numLabels) {
      throw new IllegalStateException("Different number of predictions and labels")
    }
    val predictedPositive = 1.0 * predictions.map(p => if (p == 1) 1 else 0).sum
    if (predictedPositive == 0) return 0.0
    val truePositive = 1.0 * predictions.zip(labels).map { case (p, l) => 
      if (p == 1 && l == 1) 1 else 0
    }.sum
    truePositive / predictedPositive
  }

  def computeRecall(predictions: List[Int], labels: List[Int]): Double = {
    val numPredictions = predictions.length
    val numLabels = labels.length
    if (numPredictions != numLabels) {
      throw new IllegalStateException("Different number of predictions and labels")
    }
    val allPositive = 1.0 * labels.map(p => if (p == 1) 1 else 0).sum
    if (allPositive == 0) return 1.0  // Got all possible positive labels
    val truePositive = 1.0 * predictions.zip(labels).map { case (p, l) => 
      if (p == 1 && l == 1) 1 else 0
    }.sum
    truePositive / allPositive
  }

}