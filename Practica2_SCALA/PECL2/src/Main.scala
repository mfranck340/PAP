object Main {

  def concatenarListas(x: List[Int], y: List[Int]): List[Int] = {
    x match {
      case Nil => y
      case n :: Nil => n :: y
      case head :: tail => head :: concatenarListas(tail, y)
    }
  }

  def tamLista(x:List[Int]): Int = {
    x match {
      case Nil => 0
      case _ => 1 + tamLista(x.tail)
    }
  }

  def main(args: Array[String]) {

    println("\n- CUNDY CROSH SOGA -\n")
    /*val rand = new scala.util.Random
    val a = rand.nextInt(10)
    println(a)
    println(a)
    val b = rand.between(1, 4)
    println(b)*/

    val tablero = new Tablero(5, 5, 6)
    println(tablero.getContenido())

    val x = List(1, 2, 3, 4)
    val y = List(5, 6, 7, 8)
    val z = concatenarListas(x, y)

    print("Mostrar tablero inicial")
    tablero.mostrarTablero()
    //z.
    //println(z)
  }
}