class Tablero(col:Int, fil:Int, dif:Int) {
  private val N:Int = col
  private val M:Int = fil
  private val D:Int = dif
  private val contenido:List[Int] = inicializarMatriz(N * M)

  private def inicializarMatriz(celdas:Int): List[Int] = {
     celdas match {
       case 0 => Nil
       case _ => 0 :: inicializarMatriz(celdas - 1)
     }
  }

  private def concatenarListas(x:List[Int], y:List[Int]): List[Int] = {
    x match {
      case Nil => y
      case n :: Nil => n :: y
      case head :: tail => head::concatenarListas(tail, y)
    }
  }

  private def mostrarTableroAux(x:List[Int]): Unit = {
    x match {
      case Nil =>  print("\n" + "---" * (N - 1) + "----")
      case _ =>
        if (tamLista(x) % N == 0) {
          print("\n" + "---" * (N - 1) + "----" + "\n|")
        }
        if (x.head < 10)
          print(x.head + " |")
        else
          print(x.head + "|")
        mostrarTableroAux(x.tail)
    }
  }

  private def tamLista(x:List[Int]): Int = {
    x match {
      case Nil => 0
      case _ => 1 + tamLista(x.tail)
    }
  }

  private def invertirLista(x: List[Int]): List[Int] = {
    x match {
      case Nil => Nil
      case _ => concatenarListas(invertirLista(x.tail), x.head :: Nil)
    }
  }

  def mostrarTablero(): Unit = {
    mostrarTableroAux(contenido)
  }

  def generarFichas(): Unit = {

  }

  def bajarFichas(): Unit = {

  }

  def getContenido(): List[Int] = {
    contenido
  }
}

