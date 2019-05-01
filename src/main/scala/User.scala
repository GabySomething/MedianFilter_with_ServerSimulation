import java.awt.{Font, Graphics}
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import javax.imageio.ImageIO
import Servers.{NonParallelServer, ParallelServer, Server1Response, Server2Response}
import javax.swing.{JComponent, JFileChooser, JFrame}
import javax.swing.filechooser.FileNameExtensionFilter

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object User extends App {

  def saveToNewImage(img: BufferedImage, fileName: String): Unit = {
    ImageIO.write(img, "jpg", new File("output/" + fileName + ".jpg"))
  }


  def loadImage: BufferedImage = {
    val chooser = new JFileChooser
    chooser.setDialogTitle("Select an image file")
    val filter = new FileNameExtensionFilter("JPG & PNG Images", "jpg", "png")
    chooser.setFileFilter(filter)
    if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      return ImageIO.read(chooser.getSelectedFile)
    }
    null
  }

  def copyImg(img: BufferedImage): BufferedImage = {
    val b = new BufferedImage(img.getWidth, img.getHeight, img.getType)
    val g = b.getGraphics
    g.drawImage(img, 0, 0, null)
    g.dispose()
    b
  }

  val system = ActorSystem()
  val serverSerial: ActorRef = system.actorOf(Props[NonParallelServer], name = "NonParallelServer")
  val serverParallel: ActorRef = system.actorOf(Props[ParallelServer], name = "ParallelServer")

  val image: BufferedImage = loadImage
  if (image == null) {
    System.err.println("You didn't choose an image.")
    System.exit(-1)
  }
  println("Saving chosen image in the input folder for reference.")
  ImageIO.write(image, "jpg", new File("input/ClientInput.jpg"))
  val originalImg = copyImg(image)
  implicit val timeout: Timeout = Timeout(10000.second)
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  println("Loading, please wait...")
  val mfSerial: Future[Any] = serverSerial ? image
  val mfParallel: Future[Any] = serverParallel ? image

  val gotTheImages: Future[(Any, Any)] = for {
    imageSerial <- mfSerial
    imageParallel <- mfParallel
  } yield (imageSerial, imageParallel)

  gotTheImages.onComplete {
    case Success(res) => {
      val noParallelDuration: Double = res._1.asInstanceOf[Server1Response].duration
      val parallelDuration: Double = res._2.asInstanceOf[Server2Response].duration
      val parallelImg = new filteredImage(res._2.asInstanceOf[Server2Response].image, parallelDuration.toLong)
      val nparallelImg = new filteredImage(res._1.asInstanceOf[Server1Response].image, noParallelDuration.toLong)
      println("Median Filter Serial: " + noParallelDuration.toInt + "ms")
      println("Median Filter Parallel: " + parallelDuration.toInt + "ms")
      saveToNewImage(res._1.asInstanceOf[Server1Response].image, "mfNonParallel")
      saveToNewImage(res._2.asInstanceOf[Server2Response].image, "mfParallel")
      println("The parallel server was " + math.round((noParallelDuration / parallelDuration) * 1000.0) / 1000.0 + " times faster than the non-parallel one")
      println("Images saved in the output folder.")
      println("Loading window to compare input with the outputs")
      val component: JComponent = new WindowComponent(originalImg, parallelImg, nparallelImg)
      val frame: JFrame = new JFrame()
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      frame.add(component)
      frame.setSize(100 + math.min(originalImg.getWidth * 2, 600), 100 + math.min(originalImg.getHeight * 2, 600))
      frame.setVisible(true)
    }
    case Failure(e) => e.printStackTrace()
  }


}

class WindowComponent(var input: BufferedImage, var parallel: filteredImage, var nparallel: filteredImage) extends JComponent {

  def scaleImg(img: BufferedImage, width: Double, height: Double): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    val at = new AffineTransform
    at.scale(width, height)
    val op = new AffineTransformOp(at, AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
    val after = new BufferedImage((w * width).toInt, (h * height).toInt, BufferedImage.TYPE_INT_ARGB)
    op.filter(img, after)
    after
  }

  def resizeImg(img: BufferedImage, width: Double, height: Double): BufferedImage = {
    val w = img.getWidth
    val h = img.getHeight
    scaleImg(img, width / w, height / h)
  }

  val bigFont = new Font("Arial", Font.BOLD, 20)
  val maxXSize: Double = 300.0
  val maxYSize: Double = 300.0
  var ok: Boolean = false
  if (input != null && parallel != null && nparallel != null) {
    ok = true
    if (input.getWidth > maxXSize) {
      input = scaleImg(input, maxXSize / input.getWidth, maxXSize / input.getWidth)
      parallel.setImg(scaleImg(parallel.getImg, maxXSize / parallel.getWidth, maxXSize / parallel.getWidth))
      nparallel.setImg(scaleImg(nparallel.getImg, maxXSize / nparallel.getWidth, maxXSize / nparallel.getWidth))
    }
    if (input.getHeight > maxYSize) {
      input = scaleImg(input, maxYSize / input.getHeight, maxYSize / input.getHeight)
      parallel.setImg(scaleImg(parallel.getImg, maxYSize / parallel.getWidth, maxYSize / parallel.getWidth))
      nparallel.setImg(scaleImg(nparallel.getImg, maxYSize / nparallel.getWidth, maxYSize / nparallel.getWidth))
    }
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    if (!ok)
      return
    g.setFont(bigFont)
    var string: String = "Input"
    g.drawString(string, this.getWidth / 2 - g.getFontMetrics.stringWidth(string) / 2, this.getHeight / 3 - input.getHeight / 2 - g.getFontMetrics.getHeight)
    g.drawImage(input, this.getWidth / 2 - input.getWidth / 2, this.getHeight / 3 - input.getHeight / 2, null)
    g.drawImage(nparallel.getImg, this.getWidth / 2, 2 * this.getHeight / 3 - parallel.getHeight / 2, null)
    g.drawImage(parallel.getImg, this.getWidth / 2 - parallel.getWidth, 2 * this.getHeight / 3 - parallel.getHeight / 2, null)

    string = "Parallel Server"
    g.drawString(string, this.getWidth / 2 - parallel.getWidth / 2 - g.getFontMetrics.stringWidth(string) / 2, 2 * this.getHeight / 3 - parallel.getHeight / 2 + parallel.getHeight + g.getFontMetrics.getHeight)
    string = parallel.getTime.toString
    g.drawString(string, this.getWidth / 2 - parallel.getWidth / 2 - g.getFontMetrics.stringWidth(string) / 2, 2 * this.getHeight / 3 - parallel.getHeight / 2 + parallel.getHeight + g.getFontMetrics.getHeight * 2)
    string = "Non Parallel Server"
    g.drawString(string, this.getWidth / 2 + nparallel.getWidth / 2 - g.getFontMetrics.stringWidth(string) / 2, 2 * this.getHeight / 3 - nparallel.getHeight / 2 + nparallel.getHeight + g.getFontMetrics.getHeight)
    string = nparallel.getTime.toString
    g.drawString(string, this.getWidth / 2 + nparallel.getWidth / 2 - g.getFontMetrics.stringWidth(string) / 2, 2 * this.getHeight / 3 - nparallel.getHeight / 2 + nparallel.getHeight + g.getFontMetrics.getHeight * 2)

  }

}

class filteredImage(var img: BufferedImage, time: Long) {

  def setImg(img: BufferedImage): Unit = {
    this.img = img
  }
  def getImg: BufferedImage = img
  def getWidth: Int = img.getWidth
  def getHeight: Int = img.getHeight
  def getTime: Long = time
}