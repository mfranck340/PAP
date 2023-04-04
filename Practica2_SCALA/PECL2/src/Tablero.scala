class Tablero() {
  private val rand = new scala.util.Random(System.currentTimeMillis())

  def inicializarTablero(celdas:Int): List[Int] = {
     celdas match {
       case 0 => Nil
       case _ => 0 :: inicializarTablero(celdas - 1)
     }
  }

  private def concatenarListas(x:List[Int], y:List[Int]): List[Int] = {
    x match {
      case Nil => y
      case n :: Nil => n :: y
      case head :: tail => head::concatenarListas(tail, y)
    }
  }

  private def mostrarTableroAux(x:List[Int], n:Int): Unit = {
    x match {
      case Nil =>  print("\n" + "---" * (n - 1) + "----")
      case _ =>
        if (lengthCustom(x) % n == 0) {
          print("\n" + "---" * (n - 1) + "----" + "\n|")
        }
        if (x.head < 10)
          print(x.head + " |")
        else
          print(x.head + "|")
        mostrarTableroAux(x.tail, n)
    }
  }

  private def lengthCustom(x:List[Int]): Int = {
    x match {
      case Nil => 0
      case _ => 1 + lengthCustom(x.tail)
    }
  }

  def reverseCustom(x: List[Int]): List[Int] = {
    x match {
      case Nil => Nil
      case _ => concatenarListas(reverseCustom(x.tail), x.head :: Nil)
    }
  }

  def mostrarTablero(tablero:List[Int], n:Int): Unit = {
    mostrarTableroAux(tablero, n)
  }

  def generarFichas(tablero:List[Int], dif:Int, col:Int): List[Int] = {
    col match {
      case 0 => tablero
      case _ =>
        if (tablero.head == 0)
          rand.between(1, dif + 1) :: generarFichas(tablero.tail, dif, col - 1)
        else
          tablero.head :: generarFichas(tablero.tail, dif, col - 1)
    }
  }

  private def insertar(elem: Int, pos: Int, lista: List[Int]): List[Int] = {
    lista match {
      case Nil => elem :: Nil
      case _ =>
        pos match {
          case 0 => elem :: lista.tail
          case _ => lista.head :: insertar(elem, pos - 1, lista.tail)
        }
    }
  }

  //Funcion para obtener un elemento de la matriz unidimensional
  private def getElem(index: Int, matriz: List[Int]): Int = {
    index match {
      case 0 => matriz.head
      case _ => getElem(index - 1, matriz.tail)
    }
  }

  def bajarFichas(tablero:List[Int], col:Int, pos:Int): List[Int] = {
    if (pos == col - 1)
      tablero
    else {
      if (getElem(pos, tablero) == 0 && pos - col >= 0 && getElem(pos - col, tablero) != 0)
          bajarFichas(insertar(0, pos - col, insertar(getElem(pos - col, tablero), pos, tablero)), col, pos - 1)
      else
        bajarFichas(tablero, col, pos - 1)
    }
  }

  private def comprobarTablero(tablero:List[Int]): Boolean = {
    tablero match {
      case Nil => false
      case _ =>
        if (tablero.head == 0) true
        else comprobarTablero(tablero.tail)
    }
  }

  def actualizarTableroAux(tablero:List[Int], col:Int, dif:Int, continuar:Boolean): List[Int] = {
    continuar match {
      case false => tablero
      case _ => {
        val tabAux = generarFichas(bajarFichas(tablero, col, lengthCustom(tablero) - 1), dif, col)
        actualizarTableroAux(tabAux, col, dif, comprobarTablero(tabAux))
      }
    }
  }

  def actualizarTablero(tablero:List[Int], col:Int, dif:Int): List[Int] = {
    actualizarTableroAux(tablero, col, dif, true)
  }



}
