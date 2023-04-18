import java.awt.{Color, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.swing._
import scala.swing.event._

// --------------------------------------------------------------------
//Evento para actualizar las vidas y cerrar la ventana
case class SalirEvent() extends Event

// --------------------------------------------------------------------
//Componente para dibujar el tablero en la interfaz
class Canvas(tabIn:List[Int], col:Int, fil:Int, mod:Char, dif:Int, tam:Int) extends Component {
  private val tab = new Tablero()

  private val filas = fil
  private val columnas = col
  private val dificultad = dif
  private val modo = mod
  private var tablero = tabIn

  //Inicializamos una lista con las imágenes que vamos a utilizar en la interfaz
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

  //Tamaño de las fichas del tablero (las celdas donde aparecen)
  private val anchoCelda = tam
  private val altoCelda = tam

  //Tamaño de la pantalla de juego en función de los números de filas y columnas introducidos
  preferredSize = new Dimension(anchoCelda * columnas, altoCelda * filas)

  //Controlador de eventos de clic
  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(_, p, _, _, _) =>
      val fila = p.y / altoCelda                                                  //Obtenemos la fila pulsada
      val columna = p.x / anchoCelda                                              //Obtenemos la columna pulsada
      if (fila < filas && columna < columnas && !tab.comprobarTablero(tablero) && modo == 'm') {
        tocar(columna, fila)                                                      //En el modo manual, ejecutamos el movimiento correspondiente en la casilla que ha pulsado el usuario
      }
  }

  //Función para dibujar los componentes en la interfaz
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    //Función recursiva para dibujar el tablero
    @tailrec
    def dibujarRecursivo(fila: Int, columna: Int): Unit = {
      if (fila < filas) {
        val x = columna * anchoCelda                                              //Calculamos la coordenada X de la esquina superior izquierda de la casilla actual
        val y = fila * altoCelda                                                  //Calculamos la coordenada Y de la esquina superior izquierda de la casilla actual
        g.drawImage(asignarImagen(fila * columnas + columna), x, y, anchoCelda, altoCelda, null)  //Llamamos a asignarImagen() para obtener la imagen asocada a esa posición y la dibujamos en las coordenadas calculadas
        if (columna < columnas - 1) dibujarRecursivo(fila, columna + 1)           //Nos recorremos la fila actual de manera recursiva
        else dibujarRecursivo(fila + 1, 0)                               //Nos recorremos todas las filas recursivamente
      }
    }

    dibujarRecursivo(0, 0)                                          //Dibujamos el tablero recursivamente empezando desde la primera posición

    if (tab.comprobarTablero(tablero)) {
      tablero = tab.generarFichas(tab.bajarFichas(tablero, columnas, columnas * filas - 1), dificultad, columnas)
      this.repaint()                                                              //Si hay bloques de aire, generamos fichas y las bajamos antes de redibujar el tablero
      Thread.sleep(300)                                                    //Esperamos 300 ms para ver cómo bajan las fichas lentamente
    }
    else if (modo == 'a') {                                                       //Si no hay bloques de aire y el modo de ejecución es automático, ejecutamos la función para realizar el movimiento que elimine más fichas
      val pos = tab.eliminarMasFichas(tablero, 0, 0, filas * columnas - 1, columnas, dificultad)
      val columna = pos % columnas                                                //Obtenemos la columna
      val fila = (pos - columna) / columnas                                       //Obtenemos la fila
      tocar(columna, fila)                                                        //Realizamos el movimiento
    }
  }

  //Función que realiza la acción correspondiente a la casilla que se ha pulsado
  private def tocar(ejeX:Int, ejeY:Int): Unit = {
    val (tabAux, borrar) = tab.realizarMovimiento(tablero, ejeX, ejeY, columnas, dificultad)
    if (borrar) {                                                                 //Si, tras realizar el movimiento, necesitamos restar una vida, enviamos una señal a SalirEvent()
      publish(SalirEvent())
    }
    tablero = tabAux                                                              //Actualizamos el tablero
    this.repaint()                                                                //Redibujamos el tablero
    Thread.sleep(300)                                                      //Esperamos 300 ms para ver cómo se eliminan las fichas
  }

  //Función que devuelve las imagenes asociadas a las fichas de las casillas
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
//Ventana de la interfaz
class UI() extends MainFrame {
  private val size_font = 18                                                      //Inicializamos el tamaño de la fuente
  private val tab = new Tablero()                                                 //Creamos el objeto tablero

  private var vidas = 5                                                           //Número de vidas
  private val tam_celda = 75                                                      //Inicializamos el tamaño de las celdas
  title = "Cundy Crosh Soga"                                                      //Indicamos el título del juego

  //----------------------------------------------------------------------------
  //Primer panel (Pantalla de inicio del juego)
  private val image: BufferedImage = ImageIO.read(new File("src/resources/fondo.jpg"))  //Obtenemos la imagen del fondo de pantalla
  private val startButton = new Button() {                                        //Creamos el botón para pasar a la siguiente ventana
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      obtenerDatos()                                                              //Cuando pulsamos el botón, abrimos la siguiente ventana (selección de datos)
    }
  }

  //Función para crear todos los componentes de la ventana
  private def crearPantallaInicio(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(image.getWidth, image.getHeight)              //Obtenemos las dimensiones de la imagen

      //Función para dibujar el fondo de pantalla
      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.drawImage(image, 0, 0, null)
      }

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        contents += new Label("Cundy Crosh Soga") {                               //Creamos una etiqueta con el nombre del juego
          font = new Font("Arial", java.awt.Font.BOLD, 40)
        }
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        border = Swing.EmptyBorder(150, 10, 10, 10)                               //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      contents += Swing.VStrut(100)                                               //Dejamos espacio entre los componentes
      contents += crearContenedorBoton(startButton)                               //Creamos el botón
    }
  }

  // ---------------------------------------------------------------------
  //Segundo panel (Pantalla de selección de datos del juego)
  private val colField = crearFieldDatos()                                        //Creamos el text field para obtener el número de columnas
  private val filField = crearFieldDatos()                                        //Creamos el text field para obtener el número de filas
  private val modeComboBox = new ComboBox(List("Automatico", "Manual")) {         //Creamos el combo box para seleccionar el modo de juego
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  private val difficultyComboBox = new ComboBox(List("Facil", "Dificil")) {       //Creamos el combo box para seleccionar la dificultad
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
  }
  private val continueButton = new Button() {                                     //Creamos el botón para pasar a la siguiente ventana
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Start") {
      comenzarJuego()                                                             //Cuando pulsamos el botón, abrimos la siguiente ventana (juego)
    }
  }

  //Función para crear todos los componentes de la ventana
  private def crearPantallaDatos(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(image.getWidth, image.getHeight)              //Obtenemos las dimensiones de la imagen

      //Función para dibujar el fondo de pantalla
      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.drawImage(image, 0, 0, null)
      }

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Filas: ") {                                        //Creamos una etiqueta que ponga "Filas"
          font = new Font("Arial", java.awt.Font.BOLD, size_font)
        }
        contents += Swing.HStrut(15)                                              //Dejamos espacio entre los componentes
        contents += filField
        contents += Swing.HStrut(30)                                              //Dejamos espacio entre los componentes
        contents += new Label("Columnas: ") {                                     //Creamos una etiqueta que ponga "Columnas"
          font = new Font("Arial", java.awt.Font.BOLD, size_font)
        }
        contents += Swing.HStrut(15)                                              //Dejamos espacio entre los componentes
        contents += colField
        border = Swing.EmptyBorder(10, 10, 10, 10)                                //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      contents += Swing.VStrut(20)                                                //Dejamos espacio entre los componentes
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Modo de ejecucion: ") {                            //Creamos una etiqueta que ponga "Modo de ejecución"
          font = new Font("Arial", java.awt.Font.BOLD, size_font)
        }
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        contents += modeComboBox
        border = Swing.EmptyBorder(10, 10, 10, 10)                                //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      contents += Swing.VStrut(20)                                                //Dejamos espacio entre los componentes
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Dificultad: ") {                                   //Creamos una etiqueta que ponga "Dificultad"
          font = new Font("Arial", java.awt.Font.BOLD, size_font)
        }
        contents += Swing.HStrut(92)                                              //Dejamos espacio entre los componentes
        contents += difficultyComboBox
        border = Swing.EmptyBorder(10, 10, 10, 10)                                //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      contents += Swing.VStrut(40)                                                //Dejamos espacio entre los componentes
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        contents += continueButton
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        border = Swing.EmptyBorder(10, 10, 10, 10)                                //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      border = Swing.EmptyBorder(90, 190, 80, 190)                                //Añadimos bordes
    }
  }

  //--------------------------------------------------------------------------------
  //Tercer panel (Pantalla del juego)
  private def crearPantallaGame(col:Int, fil:Int, modo:Char, dif:Int): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(col * tam_celda + 40, fil * tam_celda + 40 + 50)  //Redimensionamos la ventana al tamaño del tablero
      background = new Color(255, 64, 160)                                        //Ponemos un color de fondo
      val tablero: List[Int] = tab.inicializarTablero(col * fil)
      private val labelVidas = new Label("Vidas: " + vidas) {                     //Creamos una etiqueta que ponga "Vidas"
        font = new Font("Arial", java.awt.Font.BOLD, 25)
        foreground = new Color(255, 255, 255)
      }
      val canvas =  new Canvas(tablero, col, fil, modo, dif, tam_celda)

      contents += labelVidas
      contents += Swing.VStrut(10)                                                //Dejamos espacio entre los componentes
      contents += canvas

      listenTo(canvas)
      reactions += {
        case SalirEvent() =>                                                      //Evento para reducir vidas y acabar el juego
          vidas -= 1                                                              //Restamos una vida
          labelVidas.text = "Vidas: " + vidas                                     //Actualizamos la etiqueta de las vidas
          if (vidas == 0) {
            vidas = 5                                                             //Reiniciamos las vidas
            finJuego()                                                            //Cambiamos a la ventana de Game Over
          }
      }
      border = Swing.EmptyBorder(20, 20, 20, 20)                                  //Añadimos bordes
    }
  }

  //------------------------------------------------------------------------------------
  //Cuarto panel (Pantalla de Game Over)
  private val imagenFin: BufferedImage = ImageIO.read(new File("src/resources/end.jpg"))
  private val startGameButton = new Button() {                                    //Creamos el botón para reiniciar el juego
    font = new Font("Arial", java.awt.Font.BOLD, size_font)
    action = Action("Play Again") {
      otraVez()                                                                   //Cuando pulsamos el botón, pasamos a la primera ventana (inicio)
    }
  }

  //Función para crear todos los componentes de la ventana
  private def crearPantallaFin(): BoxPanel = {
    new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(image.getWidth, image.getHeight)              //Obtenemos las dimensiones de la imagen

      //Función para dibujar el fondo de pantalla
      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        g.drawImage(imagenFin, 0, 0, null)
      }

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        contents += new Label("GAME OVER") {                                      //Creamos una etiqueta que ponga "Game Over"
          foreground = Color.WHITE
          font = new Font("Arial", java.awt.Font.BOLD, 40)
        }
        contents += Swing.HGlue                                                   //Redistribuimos el espacio
        border = Swing.EmptyBorder(150, 10, 10, 10)                               //Añadimos bordes
        background = new Color(0, 0, 0, 0)                                        //Indicamos el fondo transparente
      }
      contents += Swing.VStrut(100)                                               //Dejamos espacio entre los componentes
      contents += crearContenedorBoton(startGameButton)
    }
  }

  //---------------------------------------------------------------------------------------

  contents = crearPantallaInicio()

  /* FUNCIONES DE CAMBIO DE PANTALLA */
  //Función para cambiar a la pantalla de obtención de datos
  private def obtenerDatos(): Unit = {
    contents = crearPantallaDatos()
  }

  //Función para cambiar a la pantalla de juego
  private def comenzarJuego(): Unit = {
    contents = crearPantallaGame(colField.text.toInt, filField.text.toInt, if (modeComboBox.item == "Automatico") 'a' else 'm', if (difficultyComboBox.item == "Facil") 4 else 6)
  }

  //Función para cambiar a la pantalla de Game Over
  private def finJuego(): Unit = {
    contents = crearPantallaFin()
  }

  //Función para cambiar a la pantalla de inicio
  private def otraVez(): Unit = {
    contents = crearPantallaInicio()
  }

  /* FUNCIONES PARA CREAR ELEMENTOS */
  //Función para crear botones
  private def crearContenedorBoton(boton:Button): BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue                                                     //Redistribuimos el espacio
      contents += boton
      contents += Swing.HGlue                                                     //Redistribuimos el espacio
      background = new Color(0, 0, 0, 0)                                          //Indicamos el fondo transparente
    }
  }

  //Función para crear campos de texto
  private def crearFieldDatos(): TextField = {
    new TextField {
      font = new Font("Arial", java.awt.Font.BOLD, size_font)              //Indicamos la fuente
      columns = 3
    }
  }

}