object BinaryPerformance {

  case class ConfusionMatrix(truePos: Int, falsePos: Int, trueNeg: Int, falseNeg: Int) {

    // PPV = TP / (TP + FP)
    // "The fraction of positive predictions that are actually positive": "Precision"
    def getPrecision(): Double = (1.0 * truePos) / (truePos + falsePos)

    // TPR = TP / (TP + FN)
    // "The fraction of positive examples that are predicted as positive": "Recall", "Sensitivity"
    def getRecall(): Double = (1.0 * truePos) / (truePos + falseNeg)

    // TNR = TN / (TN + FP)
    // "The fraction of negative examples that are predicted as negative": "Specificity"
    def getSpecificity(): Double = (1.0 * trueNeg) / (trueNeg + falsePos)

    // FPR = FP / (FP + TN)
    // "The fraction of negative examples that are predicted as positive", "Type I Error"
    def getFalsePositiveRate(): Double = (1.0 * falsePos) / (falsePos + trueNeg)

    // FNR = FN / (FN + TP)
    // "The fraction of positive examples that are predicted as negative", "Type II Error"
    def getFalseNegativeRate(): Double = (1.0 * falseNeg) / (falseNeg + truePos)

    // NPV = TN / (TN + FN)
    // "The fraction of negative predictions that are actually negative"
    def getNegativePredictiveValue(): Double = (1.0 * trueNeg) / (trueNeg + falseNeg)

    // FDR = FP / (TP + FP)
    // "The fraction of positive predictions that are actually negative"
    def getFalseDiscoveryRate(): Double = (1.0 * falsePos) / (truePos + falsePos)

    // Accuracy = (TP + TN) / (TP + FP + FN + TN)
    // "The fraction of all predictions that are correct"
    def getAccuracy(): Double = {
      (1.0 + truePos + trueNeg) / (truePos + falsePos + falseNeg + trueNeg)
    }

    private def getFBeta(beta: Double): Double = {
      val pr = getPrecision
      val rec = getRecall
      (1 + scala.math.pow(beta, 2)) * ((pr * rec) / (scala.math.pow(beta, 2) * pr + rec))
    }

    // F1 Score (beta = 1)
    // "Harmonic mean between precision and recall"
    def getF1Score(): Double = getFBeta(1)

    // F2 Score (beta = 2)
    // "Combines precision and recall, putting 2x emphasis on recall"
    def getF2Score(): Double = getFBeta(2)

  }

  case class Metrics(
    precision: Double,
    recall: Double,
    specificity: Double,
    falsePositiveRate: Double,
    falseNegativeRate: Double,
    negativePredictiveValue: Double,
    falseDiscoveryRate: Double,
    accuracy: Double,
    f1Score: Double,
    f2Score: Double
  ) {
    
    def report(): String = {
      s"""Precision: $precision
Recall: $recall
Specificity: $specificity
False Positive Rate (Type I Error): $falsePositiveRate
False Negative Rate (Type II Error): $falseNegativeRate
Negative Predictive Value: $negativePredictiveValue
False Discovery Rate: $falseDiscoveryRate
Accuracy: $accuracy
F1 Score: $f1Score
F2 Score: $f2Score
"""
    }

  }

  def computeMetrics(predictions: List[Int], labels: List[Int]): Metrics = {
    var predictedPositives = 0
    var predictedNegatives = 0
    var truePos = 0
    var falsePos = 0
    var trueNeg = 0 
    var falseNeg = 0

    predictions.zip(labels).foreach { case (p, l) =>
      if (p == 1) {
        predictedPositives += 1
        if (p == l) truePos += 1
        else falsePos += 1
      } else {
        predictedNegatives += 1
        if (p == -1) trueNeg += 1
        else falseNeg += 1
      }
    }

    // Confusion Matrix
    val cm = ConfusionMatrix(truePos, falsePos, trueNeg, falseNeg)
    Metrics(
      cm.getPrecision,
      cm.getRecall,
      cm.getSpecificity,
      cm.getFalsePositiveRate,
      cm.getFalseNegativeRate,
      cm.getNegativePredictiveValue,
      cm.getFalseDiscoveryRate,
      cm.getAccuracy,
      cm.getF1Score,
      cm.getF2Score
    )
  }

}