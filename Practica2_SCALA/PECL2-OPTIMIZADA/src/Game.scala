import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.annotation.tailrec

class Game {

  private val vidas = 5
  private val rand = new scala.util.Random(System.currentTimeMillis())
  private val tab = new Tablero

  def run(): Unit = {
    println("\n- CUNDY CROSH SOGA -\n")
    /*println("Introduce el numero de columnas del tablero: ")
    val col = scala.io.StdIn.readInt()
    println("Introduce el numero de filas del tablero: ")
    val fil = scala.io.StdIn.readInt()
    println("Introduce la dificultad del juego (1 - facil / 2 - dificil): ")*/
    //val dif = if (scala.io.StdIn.readChar() == '1')  4 else 6
    /*println("Introduce el modo de juego (a - automatico / m - manual): ")
    val mod = scala.io.StdIn.readChar()*/

    //Variables de prueba
    val col = 10
    val fil = 10
    val dif = 4
    val mod = 'a'
    val tablero = tab.inicializarTablero(fil * col)

    println("- START GAME :) - ")

    runAux(col, fil, tab.actualizarTablero(tablero, col, dif), dif, mod, vidas)

    println("\n- GAME OVER :( -\n")
  }

  //Función donde se ejecutará el juego
  //@tailrec
  private def runAux(col: Int, fil: Int, tablero: List[Int], dif: Int, mod: Char, vidas: Int): Unit = {
    tab.mostrarTablero(tablero, col) //Mostramos el tablero
    println(s"\nVIDAS: $vidas") //Mostramos las vidas
    vidas match {
      case 0 => //Si no nos quedan vidas, salimos de la función
      case _ => //Si quedan vidas, continuamos la recursividad
        val (x, y) = if (mod == 'a') getAutomaticoParalelo(tablero, col, fil, dif) else getManual(col, fil) //Comprobamos si se ejecutará de forma manual o automática y llamamos a la respectiva función para obtener las coordenadas
        val (tabAux, restar) = tab.interactuarConTablero(tablero, x, y, col, dif) //Interactuamos con el tablero utilizando las coordenadas
        runAux(col, fil, tabAux, dif, mod, if (restar) vidas - 1 else vidas) //Llamamos recursivamente a la función restando una vida si no se ha eliminado ninguna ficha
    }
  }

  //Función para obtener las coordenadas del usuario (forma manual)
  private def getManual(col: Int, fil: Int): (Int, Int) = {
    print(s"\nIntroduce la columna (0 - ${col - 1}): ") //Solicitamos la coordenada X (columna)
    val x = scala.io.StdIn.readInt()
    print(s"Introduce la fila (0 - ${fil - 1}): ") //Solicitamos la coordenada Y (fila)
    val y = scala.io.StdIn.readInt()
    println(s"Coordenadas ($x, $y)") //Mostramos las coordenadas
    (x, y) //Devolvemos una tupla con las coordenadas
  }

  //Función para obtener las coordenadas (forma automática)
  private def getAutomatico(tablero: List[Int], col: Int, fil: Int, dif: Int): (Int, Int) = {
    val pos = tab.eliminarMasFichas(tablero, 0, 0, fil * col - 1, col, dif)
    val x = pos % col
    val y = (pos - x) / col
    println(s"\nCoordenadas ($x, $y)") //Mostramos las coordenadas
    (x, y) //Devolvemos una tupla con las coordenadas
  }

  private def getAutomaticoParalelo(tablero: List[Int], col: Int, fil: Int, dif: Int): (Int, Int) = {
    val tabAux = crearTableroDePosiciones(fil * col - 1, 0).par
    val contadores = tabAux.map(x => tab.ejecutarMovimiento(tablero, x, col)).toList
    val pos = tab.buscarMejorMovimiento(contadores, 0, 0, 0)
    val x = pos % col
    val y = (pos - x) / col
    println(s"\nCoordenadas ($x, $y)") //Mostramos las coordenadas
    (x, y) //Devolvemos una tupla con las coordenadas*/
  }

  private def crearTableroDePosiciones(num: Int, cont:Int): List[Int] = {
    num match {
      case 0 => cont :: Nil
      case _ => cont :: crearTableroDePosiciones(num - 1, cont + 1)
    }
  }
}
