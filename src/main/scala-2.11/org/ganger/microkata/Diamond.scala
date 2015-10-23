package org.ganger.microkata

object Diamond {

  final val initChar: Char = 'A'

  def build(finalChar: Char): Seq[String] = {

    val halfPart:Seq[String] = half(finalChar)
    
    halfPart ++ halfPart.reverse.tail

  }

  def half(finalChar: Char): Seq[String] =
    for {
      line <- quarter(finalChar)
    } yield line ++ line.reverse.tail

  def quarter(finalChar: Char): Seq[String] = {
    val listChars = (initChar to finalChar)

    listChars
      .zipWithIndex
      .map { case (char, index) =>
        scriptLine(listChars.size - index - 1) ++ Seq(char) ++ scriptLine(index)
      }
  }

  def scriptLine(size: Int): String =
    Seq.fill(size)('-').mkString
}