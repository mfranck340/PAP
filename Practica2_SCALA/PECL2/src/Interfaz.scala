import java.awt.{Color, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.swing._
import scala.swing.event._

// --------------------------------------------------------------------
case class SalirEvent() extends Event

// --------------------------------------------------------------------
class Canvas(tabIn:List[Int], col:Int, fil:Int, mod:Char, dif:Int, tam:Int) extends Component {
  private val tab = new Tablero()

  private val filas = fil
  private val columnas = col
  private val dificultad = dif
  private val modo = mod
  private var tablero = tabIn

  private val imagenes = List(ImageIO.read(new File("src/resources/bubble.png")),
    ImageIO.read(new File("src/resources/patito_red.png")),
    ImageIO.read(new File("src/resources/patito_blue.png")),
    ImageIO.read(new File("src/resources/patito_green.png")),
    ImageIO.read(new File("src/resources/patito_yellow.png")),
    ImageIO.read(new File("src/resources/patito_pink.png")),
    ImageIO.read(new File("src/resources/patito_white.png")),
    ImageIO.read(new File("src/resources/bomba.png")),
    ImageIO.read(new File("src/resources/tnt.png")),
    ImageIO.read(new File("src/resources/rompe_red.png")),
    ImageIO.read(new File("src/resources/rompe_blue.png")),
    ImageIO.read(new File("src/resources/rompe_green.png")),
    ImageIO.read(new File("src/resources/rompe_yellow.png")),
    ImageIO.read(new File("src/resources/rompe_pink.png")),
    ImageIO.read(new File("src/resources/rompe_white.png")) )

  private val anchoCelda = tam
  private val altoCelda = tam

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

    @tailrec
    def dibujarRecursivo(fila: Int, columna: Int): Unit = {
      if (fila < filas) {
        val x = columna * anchoCelda
        val y = fila * altoCelda
        g.drawImage(asignarImagen(fila * columnas + columna), x, y, anchoCelda, altoCelda, null)
        if (columna < columnas - 1) dibujarRecursivo(fila, columna + 1)
        else dibujarRecursivo(fila + 1, 0)
      }
    }

    dibujarRecursivo(0, 0)

    if (tab.comprobarTablero(tablero)) {
      tablero = tab.generarFichas(tab.bajarFichas(tablero, columnas, columnas * filas - 1), dificultad, columnas)
      this.repaint()
      Thread.sleep(300)
    }
    else if (modo == 'a') {
      val pos = tab.eliminarMasFichas(tablero, 0, 0, filas * columnas - 1, columnas, dificultad)
      val columna = pos % columnas
      val fila = (pos - columna) / columnas
      tocar(columna, fila)
    }
  }

  private def tocar(ejeX:Int, ejeY:Int): Unit = {
    val (tabAux, borrar) = tab.realizarMovimiento(tablero, ejeX, ejeY, columnas, dificultad)
    if (borrar) {
      publish(SalirEvent())
    }
    tablero = tabAux
    this.repaint()
    Thread.sleep(300)
  }

  private def asignarImagen(pos:Int): BufferedImage = {
    tab.getElem(pos, tablero) match {
      case 0 => imagenes.head
      case 1 => imagenes(1)
      case 2 => imagenes(2)
      case 3 => imagenes(3)
      case 4 => imagenes(4)
      case 5 => imagenes(5)
      case 6 => imagenes(6)
      case 8 => imagenes(7)
      case 9 => imagenes(8)
      case 11 => imagenes(9)
      case 12 => imagenes(10)
      case 13 => imagenes(11)
      case 14 => imagenes(12)
      case 15 => imagenes(13)
      case 16 => imagenes(14)
    }
  }
}

// --------------------------------------------------------------------

class UI() extends MainFrame {

  private val size_font = 18
  private val tab = new Tablero()

  private var vidas = 5
  private val tam_celda = 75
  title = "Cundy Crosh Soga"

  //----------------------------------------------------------------------------
  //Primera ventana
  private val image: BufferedImage = ImageIO.read(new File("src/resources/fondo.jpg"))
  private val startButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      obtenerDatos()
    }
  }
  private def crearPantallaInicio(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(image.getWidth, image.getHeight)

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
      contents += crearContenedorBoton(startButton)
    }
  }

  // ---------------------------------------------------------------------
  //Segunda ventana

  private val colField = crearFieldDatos()
  private val filField = crearFieldDatos()
  private val modeComboBox = new ComboBox(List("Automatico", "Manual")) {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  private val difficultyComboBox = new ComboBox(List("Facil", "Dificil")) {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  private val continueButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      comenzarJuego()
    }
  }
  private def crearPantallaDatos(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
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
  }

  //--------------------------------------------------------------------------------
  //Tercera Ventana
  private def crearPantallaGame(col:Int, fil:Int, modo:Char, dif:Int): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(col * tam_celda + 40, fil * tam_celda + 40 + 50)
      background = new Color(255, 64, 160)
      val tablero: List[Int] = tab.inicializarTablero(col * fil)
      private val labelVidas = new Label("Vidas: " + vidas) {
        font = new Font("Arial", java.awt.Font.BOLD, 25)
        foreground = new Color(255, 255, 255)
      }
      val canvas =  new Canvas(tablero, col, fil, modo, dif, tam_celda)

      contents += labelVidas
      contents += Swing.VStrut(10)
      contents += canvas

      listenTo(canvas)
      reactions += {
        case SalirEvent() =>
          vidas -= 1
          labelVidas.text = "Vidas: " + vidas
          if (vidas == 0) {
            vidas = 5
            finJuego()
          }
      }
      border = Swing.EmptyBorder(20, 20, 20, 20)
    }
  }

  //------------------------------------------------------------------------------------
  //Ventana Game Over
  private val imagenFin: BufferedImage = ImageIO.read(new File("src/resources/end.jpg"))
  private val startGameButton = new Button() {
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Play Again") {
      otraVez()
    }
  }
  private def crearPantallaFin(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
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
      contents += crearContenedorBoton(startGameButton)
    }
  }

  //---------------------------------------------------------------------------------------

  contents = crearPantallaInicio()

  //Funciones de cambio de pantalla
  private def obtenerDatos(): Unit = {
    contents = crearPantallaDatos()
  }

  private def comenzarJuego(): Unit = {
    contents = crearPantallaGame(colField.text.toInt, filField.text.toInt, if (modeComboBox.item == "Automatico") 'a' else 'm', if (difficultyComboBox.item == "Facil") 4 else 6)
  }

  private def finJuego(): Unit = {
    contents = crearPantallaFin()
  }

  private def otraVez(): Unit = {
    contents = crearPantallaInicio()
  }

  //Funciones para crear elementos
  private def crearContenedorBoton(boton:Button): BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += boton
      contents += Swing.HGlue
      background = new Color(0, 0, 0, 0)
    }
  }

  private def crearFieldDatos(): TextField = {
    new TextField {
      font = new Font("Arial", java.awt.Font.BOLD, size_font)
      columns = 3
    }
  }

}