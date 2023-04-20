import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.annotation.tailrec

class Game(args: List[String]) {

  private val vidas = 5
  private val tab = new Tablero

  def run(): Unit = {
    println("\n- CUNDY CROSH SOGA -\n")

    val(col, fil, dif, mod) = if (lengthArgumentos(args) == 4) obtenerDatos(args) else pedirDatos()
    val tablero = tab.inicializarTablero(fil * col)

    println("- START GAME :) - ")

    runAux(col, fil, tab.actualizarTablero(tablero, col, dif), dif, mod, vidas)

    println("\n- GAME OVER :( -\n")
  }

  //Función donde se ejecutará el juego
  @tailrec
  private def runAux(col: Int, fil: Int, tablero: List[Int], dif: Int, mod: Char, vidas: Int): Unit = {
    tab.mostrarTablero(tablero, col)                                                    //Mostramos el tablero
    println(s"\nVIDAS: $vidas")                                                         //Mostramos las vidas
    vidas match {
      case 0 =>                                                                         //Si no nos quedan vidas, salimos de la función
      case _ =>                                                                         //Si quedan vidas, continuamos la recursividad

        val startTime = System.nanoTime()
        //val (x, y) = if (mod == 'a') getAutomatico(tablero, col, fil, dif) else getManual(col, fil)
        val (x, y) = if (mod == 'a') getAutomaticoParalelo(tablero, col, fil) else getManual(col, fil) //Comprobamos si se ejecutará de forma manual o automática y llamamos a la respectiva función para obtener las coordenadas
        val endTime = System.nanoTime()

        println("Time: " + (endTime - startTime))                                       //Mostramos el tiempo que tarda en ejecutarse un movimiento

        val (tabAux, restar) = tab.interactuarConTablero(tablero, x, y, col, dif)       //Interactuamos con el tablero utilizando las coordenadas
        runAux(col, fil, tabAux, dif, mod, if (restar) vidas - 1 else vidas)            //Llamamos recursivamente a la función restando una vida si no se ha eliminado ninguna ficha
    }
  }

  private def pedirDatos(): (Int, Int, Int, Char) = {
    println("Introduce el numero de columnas del tablero: ")
    val col = scala.io.StdIn.readInt()
    println("Introduce el numero de filas del tablero: ")
    val fil = scala.io.StdIn.readInt()
    println("Introduce la dificultad del juego (1 - facil / 2 - dificil): ")
    val dif = if (scala.io.StdIn.readChar() == '1') 4 else 6
    println("Introduce el modo de juego (a - automatico / m - manual): ")
    val mod = scala.io.StdIn.readChar()
    (col, fil, dif, mod)
  }

  private def obtenerDatos(args:List[String]): (Int, Int, Int, Char) =  {
    val mod = args.head(0)
    val dif = if (args(1)(0) == '1') 4 else 6
    val col = args(2).toInt
    val fil = args(3).toInt
    println(s"Datos Introducidos: columnas:$col, filas:$fil, dificultad:$dif, modo:$mod")
    (col, fil, dif, mod)
  }

  private def lengthArgumentos(args:List[String]): Int = {
    args match {
      case Nil => 0
      case _ => 1 + lengthArgumentos(args.tail)
    }
  }

  //Función para obtener las coordenadas del usuario (forma manual)
  private def getManual(col: Int, fil: Int): (Int, Int) = {
    print(s"\nIntroduce la columna (0 - ${col - 1}): ")                                 //Solicitamos la coordenada X (columna)
    val x = scala.io.StdIn.readInt()
    print(s"Introduce la fila (0 - ${fil - 1}): ")                                      //Solicitamos la coordenada Y (fila)
    val y = scala.io.StdIn.readInt()
    println(s"Coordenadas ($x, $y)")                                                    //Mostramos las coordenadas
    (x, y)                                                                              //Devolvemos una tupla con las coordenadas
  }

  //Función para obtener las coordenadas (forma automática)
  private def getAutomatico(tablero: List[Int], col: Int, fil: Int, dif: Int): (Int, Int) = {
    val pos = tab.eliminarMasFichas(tablero, 0, 0, fil * col - 1, col, dif)
    getCoord(pos, col)                                                                              //Devolvemos una tupla con las coordenadas
  }

  //Función para obtener las coordenadas (forma automática y paralela)
  private def getAutomaticoParalelo(tablero: List[Int], col: Int, fil: Int): (Int, Int) = {
    val tabAux = crearTableroDePosiciones(fil * col - 1, 0).par                   //Creamos el tablero de posiciones como una lista paralela
    val contadores = tabAux.map(x => tab.ejecutarMovimiento(tablero, x, col)).toList    //Ejecutamos la función ejecutarMovimiento() en todas las posiciones del tablero de posiciones y lo guardamos en otra lista
    val pos = tab.buscarMejorMovimiento(contadores, 0, 0, 0)        //Obtenemos la posición con el valor más alto de esa nueva lista
    getCoord(pos, col)                                                                              //Devolvemos una tupla con las coordenadas
  }

  private def getCoord(pos:Int, col:Int): (Int, Int) = {
    val x = pos % col                                                                   //Calculamos la coordenada X en función de la posición obtenida
    val y = (pos - x) / col                                                             //Calculamos la coordenada Y en función de la posición obtenida y la coordenada X
    println(s"\nCoordenadas ($x, $y)")                                                  //Mostramos las coordenadas
    (x, y)
  }

  //Función recursiva que crea una lista con el número de cada posición en sus elementos
  private def crearTableroDePosiciones(num: Int, cont:Int): List[Int] = {
    num match {
      case 0 => cont :: Nil                                                             //En el caso base, concatenamos el contador a una lista vacía
      case _ => cont :: crearTableroDePosiciones(num - 1, cont + 1)                     //Concatenamos el contador y realizamos la llamada recursiva restando 1 al número y sumando 1 al contador
    }
  }
}
