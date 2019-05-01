package Servers

import java.awt.Color
import java.awt.image.BufferedImage

import akka.actor.Actor

import scala.collection.parallel.mutable.ParArray

case class Server2Response(image: BufferedImage, duration: Long)

class ParallelServer extends Actor {

  def receive: PartialFunction[Any, Unit] = {
    case image: BufferedImage => {
      val beforeTime = System.currentTimeMillis()
      val newImage = MedianFilterParallel(image)
      val afterTime = System.currentTimeMillis()
      sender() ! Server2Response(newImage, afterTime - beforeTime)
    }
  }

  def cloneImage(image: BufferedImage): BufferedImage = {
    val newImage = new BufferedImage(image.getWidth, image.getHeight, image.getType)
    val graphics = newImage.getGraphics
    graphics.drawImage(newImage, 0, 0, null)
    graphics.dispose()
    newImage
  }

  def getNeighborsMedian(img: BufferedImage, x: Int, y: Int, width: Int, height: Int): Color = {
    val w = (width - 1) / 2
    val h = (height - 1) / 2
    val SIZE = width * height
    val pixels: ParArray[Color] = new ParArray[Color](SIZE)
    for (ij <- (0 until SIZE).par) {
      val i: Int = ij % width - w
      val j: Int = (math.floor(ij / width) - h).toInt
      if (x + i >= 0 && x + i < img.getWidth)
        if (y + j >= 0 && y + j < img.getHeight)
          pixels(ij) = new Color(img.getRGB(x + i, y + j))
    }
    var R: Array[Int] = new Array[Int](SIZE)
    var G: Array[Int] = new Array[Int](SIZE)
    var B: Array[Int] = new Array[Int](SIZE)
    var size = 0
    for (i <- 0 until SIZE) {
      val color = pixels(i)
      if (color != null) {
        R(i) = color.getRed
        G(i) = color.getGreen
        B(i) = color.getBlue
        size += 1
      }
      else {
        R(i) = -1
        G(i) = -1
        B(i) = -1
      }
    }
    R = R.sorted(Ordering.Int.reverse)
    G = G.sorted(Ordering.Int.reverse)
    B = B.sorted(Ordering.Int.reverse)
    var median = (size - 1) / 2
    if (size % 2 == 0)
      median = size / 2
    new Color(R(median), G(median), B(median))
  }

  def MedianFilterParallel(image: BufferedImage): BufferedImage = {
    val xRange = (0 until image.getWidth).par
    val yRange = (0 until image.getHeight).par
    xRange.foreach(x => {
      yRange.foreach(y => {
        val color = getNeighborsMedian(image, x, y, 3, 3)
        image.setRGB(x, y, color.getRGB)
      })
    })
    image
  }

}