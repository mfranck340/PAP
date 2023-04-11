import java.awt.{BasicStroke, Color, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, SwingUtilities}
import scala.swing._
import scala.swing.event._

// --------------------------------------------------------------------
case class SalirEvent() extends Event

// --------------------------------------------------------------------
class Canvas(tabIn:List[Int], col:Int, fil:Int, mod:Char, dif:Int, tam:Int) extends Component {
  val tab = new Tablero()
  private val filas = fil
  private val columnas = col
  private val dificultad = dif
  private val modo = mod
  private var tablero = tabIn
  private var vidas = 5

  val i0 = ImageIO.read(new File("src/resources/bubble.png"))
  val i1 = ImageIO.read(new File("src/resources/patito_red.png"))
  val i2 = ImageIO.read(new File("src/resources/patito_blue.png"))
  val i3 = ImageIO.read(new File("src/resources/patito_green.png"))
  val i4 = ImageIO.read(new File("src/resources/patito_yellow.png"))
  val i5 = ImageIO.read(new File("src/resources/patito_pink.png"))
  val i6 = ImageIO.read(new File("src/resources/patito_white.png"))
  val i7 = ImageIO.read(new File("src/resources/bomba.png"))
  val i8 = ImageIO.read(new File("src/resources/tnt.png"))
  val i9 = ImageIO.read(new File("src/resources/rompe_red.png"))
  val i10 = ImageIO.read(new File("src/resources/rompe_blue.png"))
  val i11 = ImageIO.read(new File("src/resources/rompe_green.png"))
  val i12 = ImageIO.read(new File("src/resources/rompe_yellow.png"))
  val i13 = ImageIO.read(new File("src/resources/rompe_pink.png"))
  val i14 = ImageIO.read(new File("src/resources/rompe_white.png"))

  val anchoCelda = tam
  val altoCelda = tam

  preferredSize = new Dimension(anchoCelda * columnas, altoCelda * filas)

  // Controlador de eventos de clic
  listenTo(mouse.clicks)

  reactions += {
    case MouseClicked(_, p, _, _, _) =>
      val fila = p.y / altoCelda
      val columna = p.x / anchoCelda
      if (fila < filas && columna < columnas && !tab.comprobarTablero(tablero) && modo == 'm') {
        tocar(columna, fila)
      }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    // Dibuja las celdas
    for {
      fila <- 0 until filas
      columna <- 0 until columnas
    } {
      val x = columna * anchoCelda
      val y = fila * altoCelda
      val image = asignarImagen(fila * columnas + columna, tablero)
      g.drawImage(image, x, y, anchoCelda, altoCelda, null)
    }

    if (tab.comprobarTablero(tablero)) {
      tablero = tab.generarFichas(tab.bajarFichas(tablero, columnas, columnas * filas - 1), dificultad, columnas)
      Thread.sleep(300)
      this.repaint()
    }
    else if (modo == 'a') {
      val pos = tab.eliminarMasFichas(tablero, 0, 0, filas * columnas - 1, columnas, dificultad)
      val columna = pos % columnas
      val fila = (pos - columna) / columnas
      tocar(columna, fila)
    }
  }

  def tocar(ejeX:Int, ejeY:Int): Unit = {
    val (tabAux, borrar) = tab.realizarMovimiento(tablero, ejeX, ejeY, columnas, dificultad)
    if (borrar)
      vidas -= 1
    tablero = tabAux
    Thread.sleep(300)
    this.repaint()
  }

  def asignarImagen(pos:Int, tablero:List[Int]): BufferedImage = {
    tab.getElem(pos, tablero) match {
      case 0 => i0
      case 1 => i1
      case 2 => i2
      case 3 => i3
      case 4 => i4
      case 5 => i5
      case 6 => i6
      case 8 => i7
      case 9 => i8
      case 11 => i9
      case 12 => i10
      case 13 => i11
      case 14 => i12
      case 15 => i13
      case 16 => i14
    }
  }
}


// --------------------------------------------------------------------

class UI() extends MainFrame {

  private val size_font = 18
  val tab = new Tablero()

  var col:Int = _
  var fil:Int = _
  var modo:Char = _
  var dif:Int = _
  val tam_celda = 100
  var tablero:List[Int] = _
  var canvas = new Canvas(List(0), 1, 1, 'a', 1, 1)

  title = "Cundy Crosh Soga"

  //----------------------------------------------------------------------------
  //Primera ventana
  val image:BufferedImage = ImageIO.read(new File("src/resources/fondo.jpg"))
  val startButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      obtenerDatos()
    }
  }
  val ventana1 = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(image.getWidth, image.getHeight)
    println(image.getWidth + "  -  " + image.getHeight)
    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, null)
    }

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += new Label("Cundy Crosh Soga") {
        font = new Font("Arial", java.awt.Font.BOLD, 40)
      }
      contents += Swing.HGlue
      border = Swing.EmptyBorder(150, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(100)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += startButton
      contents += Swing.HGlue
      background = new Color(0, 0, 0, 0)
    }
  }
  // ---------------------------------------------------------------------

  //Segunda ventana
  val colField = new TextField {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    columns = 3
  }
  val filField = new TextField {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    columns = 3
  }
  val modeComboBox = new ComboBox(List("Automatico", "Manual")) {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  val difficultyComboBox = new ComboBox(List("Facil", "Dificil")) {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  val continueButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      comenzarJuego()
    }
  }

  val ventana2 = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(image.getWidth, image.getHeight)
    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.drawImage(image, 0, 0, null)
    }
    //contents += new Label("Configuracion del tablero")
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Filas: ") {
        font = new Font("Arial", java.awt.Font.BOLD, size_font)
      }
      contents += Swing.HStrut(15)
      contents += filField
      contents += Swing.HStrut(30)
      contents += new Label("Columnas: ") {
        font = new Font("Arial", java.awt.Font.BOLD, size_font)
      }
      contents += Swing.HStrut(15)
      contents += colField
      border = Swing.EmptyBorder(10, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Modo de ejecucion: ") {
        font = new Font("Arial", java.awt.Font.BOLD, size_font)
      }
      contents += Swing.HGlue
      contents += modeComboBox
      border = Swing.EmptyBorder(10, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("Dificultad: ") {
        font = new Font("Arial", java.awt.Font.BOLD, size_font)
      }
      contents += Swing.HStrut(92)
      contents += difficultyComboBox
      border = Swing.EmptyBorder(10, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(40)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += continueButton
      contents += Swing.HGlue
      border = Swing.EmptyBorder(10, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }

    border = Swing.EmptyBorder(90, 190, 80, 190)
  }

  //--------------------------------------------------------------------------------
  //Tercera Ventana
  def crearPantallaGame(col:Int, fil:Int, modo:Char, dif:Int): BoxPanel = {
    //val imagenGame = ImageIO.read(new File("src/resources/game.jpg"))
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(col * tam_celda + 40, fil * tam_celda + 40)
      println(image.getWidth + "  -  " + image.getHeight)
      background = Color.PINK
      tablero = tab.inicializarTablero(col * fil)
      canvas =  new Canvas(tablero, col, fil, modo, dif, tam_celda)
      contents += canvas

      border = Swing.EmptyBorder(20, 20, 20, 20)
    }
  }


  //------------------------------------------------------------------------------------
  //Ventana Game Over
  val imagenFin: BufferedImage = ImageIO.read(new File("src/resources/end.jpg"))
  val startGameButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Play Again") {
      obtenerDatos()
    }
  }
  val ventana4 = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(image.getWidth, image.getHeight)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.drawImage(imagenFin, 0, 0, null)
    }

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += new Label("GAME OVER") {
        foreground = Color.WHITE
        font = new Font("Arial", java.awt.Font.BOLD, 40)
      }
      contents += Swing.HGlue
      border = Swing.EmptyBorder(150, 10, 10, 10)
      background = new Color(0, 0, 0, 0)
    }
    contents += Swing.VStrut(100)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += startGameButton
      contents += Swing.HGlue
      background = new Color(0, 0, 0, 0)
    }
  }
  //---------------------------------------------------------------------------------------

  contents = ventana1

  def obtenerDatos(): Unit = {
    contents = ventana2
  }

  def comenzarJuego(): Unit = {
    col = colField.text.toInt
    fil = filField.text.toInt
    modo = if (modeComboBox.item == "Automatico") 'a' else 'm'
    dif = if (difficultyComboBox.item == "Facil") 4 else 6
    println(col + " " + fil + " " + modo + " " + dif)
    contents = crearPantallaGame(col, fil, modo, dif)
  }

  def updateTablero(): Unit = {
    println("VAMOSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS")
    tablero = tab.generarFichas(tab.bajarFichas(tablero, col, col * fil - 1), dif, col)
    canvas.repaint()
  }

  listenTo(canvas)
  reactions += {
    case SalirEvent() =>
      println("VAMOSSSSSSSS")
      tablero = tab.generarFichas(tab.bajarFichas(tablero, col, col * fil - 1), dif, col)
      updateTablero()

  }

}

object CundyCroshSoga {
  def main(args: Array[String]): Unit = {
    val ui = new UI()
    ui.centerOnScreen()
    ui.visible = true
    ui.resizable = false
  }
}