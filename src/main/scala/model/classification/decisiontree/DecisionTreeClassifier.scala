/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import model.classification.decisiontree.DecisionTreeClassifier.getSplit
import org.apache.logging.log4j.Level

import scala.collection.mutable.ListBuffer
import scala.util.Random

class DecisionTreeClassifier(val maxDepth: Int = MaxDepth,
                             override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val dtc: Level = Level.forName("decisiontreeclassifier", LogLevelSeed)

  var root: Node = new LeafNode(1, None)

  override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer())

  // Implements the ID3 learning algorithm
  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val featureIndices: List[Int] = examples.head.X.indices.toList
    var nodes: ListBuffer[Node] = ListBuffer[Node](root)
    val (x: List[List[Double]], y: List[Double]) = examples.map(ex => (ex.X, ex.y)).unzip
    nodes.foreach { node =>
      val features = node.unusedFeatures(featureIndices)  // Only use features not used in parent nodes
      val featureThresholds = for (f <- features; t <- Thresholds) yield (f, t)
      var bestSplit = getSplit(examples, featureThresholds.head._1, featureThresholds.head._2)
      featureThresholds.tail.foreach { case (feature, threshold) =>
        val split = getSplit(examples, feature, threshold)
        val splitEntropy = split.getEntropy
        logger(dtc, s"feature: $feature, threshold: $threshold, splitEntropy: $splitEntropy")
        if (splitEntropy < bestSplit.getEntropy) {
          bestSplit = split
          logger(dtc, s"updated split entropy: ${bestSplit.getEntropy}")
        }
      }
      /**
       * Algorithm stops if any of these conditions are met:
       * 1. All examples are correctly classified
       * 2. No features left to split on
       * 3. Split reduces entropy less than some epsilon
       * 4. Tree reaches some maximum depth
       */
      val someExamplesIncorrect = bestSplit.getEntropy > 0.0
      val featuresLeft = features.nonEmpty
      val entropyDropStillBig = node.parent match {
        case Some(p) => p.split.get.getEntropy - bestSplit.getEntropy < EntropyEpsilon
        case None => true
      }
      val notAtMaximumDepth = node.depth == MaxDepth
      logger(dtc, s"someExamplesIncorrect: $someExamplesIncorrect, featuresLeft: $featuresLeft, " +
        s"entropyStillDropping: $entropyDropStillBig, notAtMaximumDepth: $notAtMaximumDepth")
      if (someExamplesIncorrect && featuresLeft && entropyDropStillBig && notAtMaximumDepth) {
        logger(dtc, "node is a non-leaf node, changing from leaf node")
        val nonLeafNode = new NonLeafNode(node.depth, node.parent, None, None, Option(bestSplit))
        val leftChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode))
        val rightChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode))
        nonLeafNode.leftChild = Option(leftChild)
        nonLeafNode.rightChild = Option(rightChild)
        nodes += leftChild
        nodes += rightChild
        if (node == root) {
          logger(dtc, "Updating root node...")
          root = nonLeafNode
        }
        logger(dtc, "adding leftChild and rightChild as leaf nodes")
      }
    }
  }

  override def inference(x: List[Double]): Double = {
    var cur: Node = root
    while (!cur.isLeaf) {
      val value = cur.evaluate(x)
      cur = {
        cur match {
          case c: NonLeafNode => if (value == 0.0) c.leftChild.get else c.rightChild.get
          case l: LeafNode => l
        }
      }
    }
    cur.evaluate(x)
  }

}

object DecisionTreeClassifier {

  def getSplit(examples: List[BinaryClassificationExample], featureIndex: Int, threshold: Double): Split = {
    var (leftPos, leftNum, rightPos, rightNum) = (0, 0, 0, 0)
    examples.foreach { example =>
      if (example.X(featureIndex) >= threshold) { leftPos += example.y.toInt; leftNum += 1 }
      else { rightPos += example.y.toInt; rightNum += 1 }
    }
    Split(featureIndex, threshold, leftPos, rightPos, leftNum, rightNum)
  }

  def entropy(yHat: Double): Double = {
    -1.0 * yHat * math.log(yHat) - (1 - yHat) * math.log(1 - yHat)
  }

  def weightedEntropy(leftPos: Int, rightPos: Int, leftEntropy: Double, rightEntropy: Double): Double = {
    val numTotal = leftPos + rightPos
    1.0 * leftPos / numTotal * leftEntropy + 1.0 * rightPos / numTotal * rightEntropy
  }

}
