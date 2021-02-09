// Nathan Gillette
// Scala Main Project
// CSC 344

import scala.io.StdIn.readLine
abstract class S
case class E(left: T, right: Option[E2]) extends S
case class E2(left: E3) extends S
case class E3(left: T, right: Option[E2]) extends S
case class T(left: F, right: Option[T2]) extends S
case class T2(left: F, right: Option[T2]) extends S
case class F(left: A, right: Option[F2]) extends S
case class F2(left: Option[F2]) extends S
abstract class A extends S
case class A2(left: E) extends A
case class C(left: Char) extends A

// allows for the parsing of the input pattern
class mkPattern (pattern: String, word: Int){
  var currentWord: Int = word
  var strPattern: String = pattern

  //  increments the index of current words
  def incWords() = {
    currentWord = currentWord +1
  }

  // decrements to get the previous word in the pattern
  def decWord(): Char = {
    strPattern.charAt(currentWord-1)
  }

  // compare the word to the pattern
  def check(): Char = {
    strPattern.charAt(currentWord)
  }

  def parseS(): S = parseE()

  def parseE(): E = E(parseT(), parseE2())

  def parseE2(): Option[E2]= {
    if (currentWord < strPattern.length() && check() == ')'){
      incWords()
      None
    } else if (currentWord < strPattern.length() && check == '|'){
      incWords()
      Some(E2(parseE3()))
    } else None
  }

  def parseE3(): E3 = E3(parseT(), parseE2())

  def parseT(): T = T(parseF(), parseT2())

  def parseT2(): Option[T2] = {
    if(currentWord < strPattern.length() && check() != '|' && check() != '?' && check() != ')'){
      Some(T2(parseF(), parseT2()))
    } else None
  }

  def parseF(): F = F(parseA(), parseF2())

  def parseF2(): Option[F2] = {
    if (currentWord < strPattern.length() && check() == '?'){
      incWords()
      Some(F2(parseF2()))
    } else None
  }

  def parseA(): A = {
    if (check() == '(') {
      incWords()
      A2(parseE())
    } else{
      incWords()
      C(decWord())
    }
  }

}

object Main {
  def main(args: Array[String]) = {
    val pattern: String = readLine("pattern? ")
    val parsed = new mkPattern(pattern, 0)
    val parsedTree = parsed.parseS()
    //    println(parsedTree)
    var string = readLine("string? ")

    while (string != "stop") {
      var isMatch: Boolean = evaluate(string, parsedTree)
      if (isMatch) {
        println("match")
      } else println("no match")
      string = readLine("string? ")
    }
  }

  def evaluate(inputString: String, parsedTree: S): Boolean = {
    var index = 0

    def recurDecent(x: Any): Boolean = x match { // recursion to break down the insides of each decent, returning booleans if match or not.

      case x: E => // setting index and last so that we can keep track of where we are.
        val last = index
        val l: Boolean = recurDecent(x.left) // get a val for the left decent as either be true or false
        x.right match {
          case Some(r) =>
            if (!l) { // if left is false then return right
              index = last
              recurDecent(r)
            }
            else true // return true cause left was true as E is an OR statement
          case None => l
        }

      case x: E2 => recurDecent(x.left) // either E3 or None

      case x: E3 => // same as E but for E3
        val last = index
        val l: Boolean = recurDecent(x.left)
        x.right match {
          case Some(r) =>
            if (!l) {
              index = last
              recurDecent(r)
            }
            else true
          case None => l
        }

      case x: T => // if a "?" exists search the right if true , if right is false then go to the left, else no "?" the just go left
        x.right match {
          case Some(r) =>
            val last = index
            if (x.left.right.nonEmpty && recurDecent(r))
              true
            else
            {
              index = last
              recurDecent(x.left) && recurDecent(r)
            }
          case None => recurDecent(x.left)
        } //

      case x: T2 =>
        x.right match {
          case Some(r) =>
            val last = index
            if (x.left.right.nonEmpty && recurDecent(r))
              true
            else
            {
              index = last
              recurDecent(x.left) && recurDecent(r)
            }
          case None => recurDecent(x.left)
        }

      case x: F =>
        x.right match {
          case Some(r) =>
            val last = index
            val l = recurDecent(x.left)
            if (!l)
              index = last
            true
          case None => recurDecent(x.left)
        }

      case x: F2 => true

      case x: A2 => recurDecent(x.left)

      case x: C =>
        if (index >= inputString.length) { // if the tree is bigger than the input string then false
          false
        }
        else if (x.left == '.') { // check if the left char is == ".", so just return true as it is considered any Char
          index += 1
          true
        }
        else if (x.left == inputString.charAt(index)) { // check if the left char is a match to the current word at the index
          index += 1
          true
        } else false
    }

    //
    val result = recurDecent(parsedTree)
    if (index < inputString.length) false // checking if the string goes pass the OR bar in the pattern
    else result
  }
}
