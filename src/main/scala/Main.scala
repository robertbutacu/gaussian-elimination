package test

import first.lab.structures.RegularMatrix


object Main extends App {
  val matrix = RegularMatrix((1 to 9).map(i => List.fill(9)(i)).toList)

}
