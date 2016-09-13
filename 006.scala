import scala.swing._
import java.awt.{Color}
import java.awt.image.BufferedImage
import scala.io.Source

/**
 * Day 6: light grid
 */

class Display(cols:Int, rows:Int) {
   private val buffer = new BufferedImage(cols, rows, BufferedImage.TYPE_INT_RGB)
   private val panel = new Panel {
      override def paint(graphics:Graphics2D) {
         graphics.drawImage(buffer, 0, 0, null)
      } 
      preferredSize = new Dimension(cols, rows)
   }
   private val frame = new MainFrame {
      title = "Advent of Code - Day 6: Light grid"
      contents = panel
      centerOnScreen()
   }
   //part1: so that we can do lights(x)(y)*255
   implicit def bool2int(b:Boolean) = if (b) 1 else 0

   // Quantize an image function and draw into a pixel buffer
   def rasterize(lights: Array[Array[Int]]) = {
     for (x <- 0 to cols - 1; y <- 0 to rows - 1){
       val f = lights(x)(y)*6
       buffer.setRGB(x, y, new Color(f, f, f).getRGB())
     }
   }

   def repaint() = panel.repaint()
   def open() = frame.open()
}

class BitGrid(cols:Int, rows:Int){
   //part1: var lights = Array.fill[Boolean](cols,rows) { false }
   var lights = Array.fill[Int](cols,rows) { 0 }
   val display = new Display(cols, rows)
   display.open()
   
   def turn(from_col:Int, from_row:Int, to_col:Int, to_row:Int, by:Int) = {
     for (x <- from_col to to_col; y <- from_row to to_row){
       lights(x)(y) += by
       if (lights(x)(y) < 0) {
         lights(x)(y) = 0
       }
     }
     show()
   }

   def toggle(from_col:Int, from_row:Int, to_col:Int, to_row:Int) = {
     for (x <- from_col to to_col; y <- from_row to to_row)
       //lights(x)(y) ^= true;
       show()
   }

   def check(): Int = {
     //part1: lights.map(_.count(_ == true)).sum
     lights.map(_.sum).sum
   }

   def show() = {
     display.rasterize(lights)
     display.repaint()
   }
}

object Day06 {

  def time[F](f: => F) = {
    val t0 = System.nanoTime
    val ans = f
    printf("Elapsed: %.3f\n",1e-9*(System.nanoTime-t0))
    ans
  }

  def ReadInput = {
    val file = Source.fromFile("input") 
    file.getLines.toList
  }

  def processRules(s: List[String], grid: BitGrid): Unit = {
    val pattern = "([^\\d]+) (\\d+),(\\d+) through (\\d+),(\\d+)".r
    s.foreach { line =>
      line match {
        case pattern("turn on", beginx, beginy, endx, endy)  => grid.turn(beginx.toInt, beginy.toInt, endx.toInt, endy.toInt, 1)
        case pattern("turn off", beginx, beginy, endx, endy) => grid.turn(beginx.toInt, beginy.toInt, endx.toInt, endy.toInt, -1)
        case pattern("toggle", beginx, beginy, endx, endy)   => grid.turn(beginx.toInt, beginy.toInt, endx.toInt, endy.toInt, 2)
        case _ => // no match
      }
    }
  }

  def main(args: Array[String]) = {
    val listOfLines = ReadInput
    val grid = new BitGrid(1000, 1000)
    time (processRules(listOfLines, grid))
    grid.show()
    println(grid.check())
    //answers
    //part1: 377891
    //part2: 14110788
  }

}
