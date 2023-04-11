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
    val col = 4
    val fil = 4
    val dif = 4
    val mod = 'a'
    val tablero = tab.inicializarTablero(fil * col)

    println("- START GAME :) - ")

    runAux(col, fil, tab.actualizarTablero(tablero, col, dif), dif, mod, vidas)

    println("\n- GAME OVER :( -\n")
  }

  @tailrec
  private def runAux(col:Int, fil:Int, tablero:List[Int], dif:Int, mod:Char, vidas:Int): Unit = {
    tab.mostrarTablero(tablero, col)
    println(s"\nVIDAS: $vidas")
    vidas match {
      case 0 =>
      case _ =>
        val (x, y) = if (mod == 'a') getAutomatico(col, fil) else getManual(col, fil)
        val (tabAux, restar) = tab.interactuarConTablero(tablero, x, y, col, dif)
        runAux(col, fil, tabAux, dif, mod, if (restar) vidas - 1 else vidas)
    }
  }

  private def getManual(col:Int, fil:Int): (Int, Int) = {
    print(s"\nIntroduce la columna (0 - ${col - 1}): ")
    val x = scala.io.StdIn.readInt()
    print(s"Introduce la fila (0 - ${fil - 1}): ")
    val y = scala.io.StdIn.readInt()
    println(s"Coordenadas ($x, $y)")
    (x, y)
  }

  private def getAutomatico(col:Int, fil:Int): (Int, Int) = {
    val x = rand.nextInt(col)
    val y = rand.nextInt(fil)
    println(s"\nCoordenadas ($x, $y)")
    (x, y)
  }
}
