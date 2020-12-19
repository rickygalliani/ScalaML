/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import org.apache.logging.log4j.Level

import scala.collection.mutable
import scala.util.Random

class DecisionTreeClassifier(val maxDepth: Int = MaxDepth,
                             override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val dtc: Level = Level.forName("decisiontreeclassifier", LogLevelSeed)

  var root: Node = new RootNode

  override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer())

  // Return true if the algorithm should stop, false otherwise
  def evaluateStoppingCriteria(node: Node, bestSplit: Split, featureIndices: List[Int]): Boolean = {
    val someIncorrect = bestSplit.entropy > EqualityDelta
    val nonEmptySplit = bestSplit.size > 0
    val numFeaturesLeft = featureIndices.size
    val featuresLeft = numFeaturesLeft > 1  // will remove current feature we split again
    val aboveMaxDepth = node.depth < maxDepth
    val stop = !Seq(someIncorrect, nonEmptySplit, featuresLeft, aboveMaxDepth).forall(identity)
    if (verbose) {
      logStopDecision(
        someIncorrect, nonEmptySplit, numFeaturesLeft, featuresLeft, node.depth, maxDepth, aboveMaxDepth, stop
      )
    }
    stop
  }

  // Implements the ID3 learning algorithm
  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val featureIndices: List[Int] = examples.head.X.indices.toList
    val trainNodes = mutable.Queue(TrainNode(root, examples, featureIndices))
    while(trainNodes.nonEmpty) {
      val curNode = trainNodes.dequeue
      // Consider each feature/threshold combination
      val featureThresholds = for (f <- curNode.featureIndices; t <- Thresholds) yield (f, t)
      var bestSplit = Split(curNode.examples, featureThresholds.head._1, featureThresholds.head._2)
      featureThresholds.tail.foreach { case (feature, threshold) =>
        val split = Split(curNode.examples, feature, threshold)
        if (split.entropy < bestSplit.entropy) bestSplit = split
      }
      if (verbose) { logNode(curNode, bestSplit) }
      val stop = evaluateStoppingCriteria(curNode.node, bestSplit, curNode.featureIndices)
      if (!stop) {
        // Change current node from leaf node to non-leaf node
        val nonLeafNode = new NonLeafNode(curNode.node.depth, curNode.node.parent, split = Option(bestSplit))
        val (leftChild, rightChild) = nonLeafNode.getChildren(bestSplit.leftYHat, bestSplit.rightYHat)
        // Update the root node of the tree, if necessary
        if (curNode.node == root) { root = nonLeafNode }
        // Eliminate current feature from set of features for downstream nodes
        val downstreamFeatures = curNode.featureIndices.toSet.diff(Set(bestSplit.featureIndex)).toList
        val leftTrainNode = TrainNode(leftChild, bestSplit.leftExamples, downstreamFeatures)
        val rightTrainNode = TrainNode(rightChild, bestSplit.rightExamples, downstreamFeatures)
        trainNodes.addAll(Seq(leftTrainNode, rightTrainNode))
      } else {
        // Stopping, update the root node with a terminal value, if necessary
        if (curNode.node == root) root = new LeafNode(1, None, Option(bestSplit.yHat))
      }
    }
  }

  override def inference(X: List[Double]): Double = {
    var cur: Node = root
    while (!cur.isLeaf) {
      cur = cur match {
        case c: NonLeafNode => if (cur.evaluate(X) < EqualityDelta) c.leftChild.get else c.rightChild.get
        case l: LeafNode => l
      }
    }
    cur.evaluate(X)
  }

  def logNode(trainNode: TrainNode, split: Split): Unit = {
    logger(dtc,
      s"""\n\nAdded node to decision tree:
         |\t- depth: ${trainNode.node.depth}
         |\t- features left: ${trainNode.featureIndices.size}
         |\t- feature: ${split.featureIndex}
         |\t- threshold: ${split.threshold}
         |\t- leftPositives: ${split.leftPositives}
         |\t- leftSize: ${split.leftSize}
         |\t- rightPositives: ${split.rightPositives}
         |\t- rightSize: ${split.rightSize}
         |\t- entropy: ${split.entropy}\n""".stripMargin)
  }

  def logStopDecision(someIncorrect: Boolean,
                      nonEmptySplit: Boolean,
                      numFeaturesLeft: Int,
                      featuresLeft: Boolean,
                      depth: Int,
                      maxDepth: Int,
                      aboveMaxDepth: Boolean,
                      stop: Boolean): Unit = {
    logger(dtc,
      s"""\n\nStopping condition checks (stopping: $stop):
         |\t1) All examples are correct: ${!someIncorrect}
         |\t2) Empty split: ${!nonEmptySplit}
         |\t3) No features left to split on: ${!featuresLeft} (features left: $numFeaturesLeft)
         |\t4) Below max depth ($maxDepth): ${!aboveMaxDepth} (node depth: $depth)\n""".stripMargin)
  }

}

case class TrainNode(node: Node, examples: List[BinaryClassificationExample], featureIndices: List[Int])
