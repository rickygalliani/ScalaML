/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.decisiontree

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import org.apache.logging.log4j.Level

import scala.collection.mutable.ListBuffer
import scala.util.Random

class DecisionTreeClassifier(val maxDepth: Int = MaxDepth,
                             override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val lr: Level = Level.forName("decisiontreeclassifier", LogLevelSeed)

  val root: NonLeafNode = new NonLeafNode(1)

  override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer())

  // Implements the ID3 learning algorithm
  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)

    var featureIndices: List[Int] = examples.head.X.indices.toList
    var nodes: ListBuffer[NonLeafNode] = ListBuffer[NonLeafNode](root)

    val (X: List[List[Double]], y: List[Double]) = examples.map(ex => (ex.X, ex.y)).unzip

    nodes.foreach { node =>
      // Choose feature/threshold combination with lowest entropy
      var bestSplitEntropy = DecisionTreeClassifier.entropy(0.5)  // worst possible entropy
      featureIndices.foreach { featureIndex =>
        Thresholds.foreach { threshold =>
          val splitEntropy = DecisionTreeClassifier.splitEntropy(featureIndex, threshold, examples)
          if (splitEntropy < bestSplitEntropy) {
            bestSplitEntropy = splitEntropy
            node.featureIndex = Option(featureIndex)
            node.threshold = Option(threshold)
          }
        }
      }
      // Remove selected feature so it's not selected in lower nodes
      featureIndices = featureIndices.filter(_ != node.featureIndex.get)
      /**
       * Algorithm stops if any of these conditions are met:
       * 1. All examples are correctly classified
       * 2. No features left to split on
       * 3. Split reduces entropy less than some epsilon
       * 4. Tree reaches some maximum depth
       */
      val someExamplesIncorrect = y.zip(predictBatch(X)).forall { case (y, yHat) => y == yHat }
      val featuresLeft = featureIndices.nonEmpty
      val entropyDropStillBig = false // TODO(ricky)
      val notAtMaximumDepth = node.depth == MaxDepth
      if (someExamplesIncorrect && featuresLeft && entropyDropStillBig && notAtMaximumDepth) {
        val leftChild = new NonLeafNode(node.depth + 1)
        val rightChild = new NonLeafNode(node.depth + 1)
        node.leftChild = Option(leftChild)
        node.rightChild = Option(rightChild)
        nodes += leftChild
        nodes += rightChild
      }
    }
  }

  override def inference(x: List[Double]): Double = {
    var cur = root
    while (!cur.isLeaf) {
      val value = cur.evaluate(x)
      cur = if (value == 0.0) cur.leftChild.get else cur.rightChild.get
    }
    cur.evaluate(x)
  }

}

object DecisionTreeClassifier {

  def entropy(yHat: Double): Double = {
    -1.0 * yHat * math.log(yHat) - (1 - yHat) * math.log(1 - yHat)
  }

  def weightedEntropy(numLeft: Int, numRight: Int, leftEntropy: Double, rightEntropy: Double): Double = {
    val numTotal = numLeft + numRight
    1.0 * numLeft / numTotal * leftEntropy + 1.0 * numRight / numTotal * rightEntropy
  }

  def splitEntropy(featureIndex: Int, threshold: Double, examples: List[BinaryClassificationExample]): Double = {
    val leftYs = new ListBuffer[Double]()
    val rightYs = new ListBuffer[Double]()
    examples.foreach { example =>
      if (example.X(featureIndex) >= threshold) { leftYs += example.y }
      else { rightYs += example.y }
    }
    val numLeft = leftYs.size
    val numRight = rightYs.size
    val leftSum = leftYs.sum
    val rightSum = rightYs.sum
    val leftYHat = 1.0 * leftSum / numLeft
    val rightYHat = 1.0 * rightSum / numRight
    if (leftSum == numLeft || rightSum == numRight) { 0.0 }  // perfect split -> no entropy
    else { weightedEntropy(numLeft, numRight, entropy(leftYHat), entropy(rightYHat)) }
  }

}
