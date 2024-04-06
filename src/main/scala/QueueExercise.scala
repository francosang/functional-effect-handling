import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std._

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File
import java.awt.color.ColorSpace
import java.awt.image.ColorConvertOp

import scala.concurrent.duration.DurationInt

object QueueExercise extends IOApp {
  case class ImageInfo(filepath: String, image: BufferedImage)

  def processImage(imageInfo: ImageInfo): ImageInfo = {
    val colorOp =
      new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
    val processedImage = colorOp.filter(imageInfo.image, imageInfo.image)
    imageInfo.copy(image = processedImage)
  }

  def saveImage(image: ImageInfo): IO[Unit] = {
    IO.println(s"Saving: ${image.filepath}") *> IO
      .blocking {
        val fp = image.filepath
        val newPath = s"${fp.substring(0, fp.length - 4)}_processed.jpg"
        ImageIO.write(image.image, "jpg", new File(s"$newPath"))
      }
      .void
      .handleError(er => IO.println(s"Error saving: ${er.getMessage}"))

  }

  // please make sure directory exists
  def loadImages(directory: String): IO[List[ImageInfo]] = {
    for {
      dir <- IO.blocking(new File(directory))
      files <- IO.blocking(
        dir.listFiles.toList.filter(f =>
          f.isFile && f.getName.endsWith(".jpeg")
        )
      )
      images <- files.parTraverse(f =>
        IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f)))
      )
      _ <- IO.println(s"Number of images in $directory: ${images.length}")
    } yield images

  }

  // TODO: Take processed images from the processed queue, and save them to the corresponding file
  def imageSaver(
      processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = {
    processedImageQueue.take
      .flatMap(saveImage)
      .handleError(er => IO.println(s"Error saver: ${er.getMessage}"))
  }

  // TODO: Take raw images from the raw queue, process them and put them in the processed queue
  def imageProcessor(
      rawImageQueue: Queue[IO, ImageInfo],
      processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = {
    rawImageQueue.take
      .flatMap(it =>
        processedImageQueue.offer(processImage(it))
          *> IO.println(s"Processing image: ${it.filepath}")
      )
      .handleError(er => IO.println(s"Error processing: ${er.getMessage}"))
  }

  // TODO: Load images from the dir and put them in the queue
  def imageLoader(
      srcDirectory: String,
      rawImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = {
    loadImages(srcDirectory).flatMap(infoes =>
      infoes.parTraverse_(it =>
        rawImageQueue.offer(it) *> IO.println(s"Offering: ${it.filepath}")
      )
    )
  }

  // TODO: Create the loaders, savers and processors and get them all running!
  def start(
      sourceDirs: List[String],
      noProcessors: Int,
      noSavers: Int
  ): IO[Unit] = {
    for {
      in <- Queue.unbounded[IO, ImageInfo]
      out <- Queue.unbounded[IO, ImageInfo]
      loaders = sourceDirs.map(imageLoader(_, in))
      processors = (0 to noProcessors).map(_ =>
        imageProcessor(in, out).foreverM
      )
      savers = (0 to noSavers).map(_ => imageSaver(out).foreverM)
      _ <- (loaders ++ processors ++ savers).parSequence_
    } yield ()

  }

  override def run(args: List[String]): IO[ExitCode] = {
    val dirs = List("pics/kittens", "pics/puppies")
    start(dirs, 16, 16).timeoutTo(30.seconds, IO.unit).as(ExitCode.Success)
  }
}
