

package tokenizer

import scala.io._
import scala.swing._
import java.io._
import scala.annotation.tailrec
import scala.annotation.tailrec
import scala.swing.FileChooser.Result.Approve
import scala.swing.FileChooser.Result.Cancel
import scala.swing.FileChooser.Result.Error
import scala.annotation.tailrec

object Tokenizer {

  type Token = (TokenType, String)
  type Counts = scala.collection.mutable.Map[TokenType, Int]

  abstract class TokenType
  case object IntegerType extends TokenType
  case object FloatingPointType extends TokenType
  case object CharacterType extends TokenType
  case object StringType extends TokenType
  case object IdType extends TokenType
  case object KeywordType extends TokenType
  case object SymbolType extends TokenType
  case object CommentType extends TokenType
  case object DelimiterType extends TokenType
  case object EofType extends TokenType
  case object ErrorType extends TokenType

  val keywords = ("abstract case catch def do else extends false final " +
    "finally for forSome if implicit import lazy match " +
    "new null object override package private protected " +
    "requires return sealed super this throw trait try " +
    "true type val var while with yield _ : = => <- <: " +
    "<% >: # @ \u21D2 \u2190").split(' ').toSet   //> keywords  : scala.collection.immutable.Set[String] = Set(for, false, packag
                                                  //| e, lazy, this, try, protected, private, return, if, =, do, override, else, 
                                                  //| abstract, super, =>, yield, finally, ⇒, null, object, match, @, <%, impor
                                                  //| t, forSome, implicit, final, trait, #, new, <-, while, with, ←, _, true, 
                                                  //| >:, extends, requires, def, <:, case, type, :, catch, throw, sealed, var, v
                                                  //| al)

  val Eof = ((EofType, ""), "")                   //> Eof  : ((tokenizer.Tokenizer.EofType.type, String), String) = ((EofType,"")
                                                  //| ,"")

  val decimalNumeral = """0|[1-9][0-9]*"""        //> decimalNumeral  : String = 0|[1-9][0-9]*
  val hexNumeral = "0[xX][0-9a-fA-F]+"            //> hexNumeral  : String = 0[xX][0-9a-fA-F]+
  val octalNumeral = "0[0-7]+"                    //> octalNumeral  : String = 0[0-7]+

  val exponentPart = """[Ee][+-]?\d+"""           //> exponentPart  : String = [Ee][+-]?\d+
  val floatType = """[FfDd]"""                    //> floatType  : String = [FfDd]
  val fp1 = """\d+\.\d*(%s)?%s?""".format(exponentPart, floatType)
                                                  //> fp1  : String = \d+\.\d*([Ee][+-]?\d+)?[FfDd]?
  val fp2 = """\.\d+(%s)?%s?""".format(exponentPart, floatType)
                                                  //> fp2  : String = \.\d+([Ee][+-]?\d+)?[FfDd]?
  val fp3 = """\d+(%s)%s?""".format(exponentPart, floatType)
                                                  //> fp3  : String = \d+([Ee][+-]?\d+)[FfDd]?
  val fp4 = """\d+(%s)?%s""".format(exponentPart, floatType)
                                                  //> fp4  : String = \d+([Ee][+-]?\d+)?[FfDd]

  val upper = """[A-Z$_]"""                       //> upper  : String = [A-Z$_]
  val lower = """[a-z]"""                         //> lower  : String = [a-z]
  val letter = """[a-zA-Z$_]"""                   //> letter  : String = [a-zA-Z$_]
  val op = """[!#$%&*+-/:<=>?@\^|~]+""" // XXX do I need to escape some chars?
                                                  //> op  : String = [!#$%&*+-/:<=>?@\^|~]+
  val idrest = """[a-zA-Z$_0-9]*(_%s)?""".format(op)
                                                  //> idrest  : String = [a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?
  val varid = lower + idrest                      //> varid  : String = [a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?
  val plainid = """(%s%s|%s|%s)""".format(upper, idrest, varid, op)
                                                  //> plainid  : String = ([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[a-z][
                                                  //| a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@\^|~]+)
  val id = """(?s)(%s|`[^`]+`)(.*)""".format(plainid)
                                                  //> id  : String = (?s)(([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[a-z][
                                                  //| a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@\^|~]+)|`[^`]+`)(.*
                                                  //| )

  val SkipWhitespace = """(?s)\s+(.*)""".r        //> SkipWhitespace  : scala.util.matching.Regex = (?s)\s+(.*)
  val IntegerLiteral = """(?s)(((%s)|%s|%s)([Ll]?))(.*)""".format(hexNumeral, octalNumeral, decimalNumeral).r
                                                  //> IntegerLiteral  : scala.util.matching.Regex = (?s)(((0[xX][0-9a-fA-F]+)|0[0
                                                  //| -7]+|0|[1-9][0-9]*)([Ll]?))(.*)
  val FloatingPointLiteral = "(?s)(%s|%s|%s|%s)(.*)".format(fp1, fp2, fp3, fp4).r
                                                  //> FloatingPointLiteral  : scala.util.matching.Regex = (?s)(\d+\.\d*([Ee][+-]?
                                                  //| \d+)?[FfDd]?|\.\d+([Ee][+-]?\d+)?[FfDd]?|\d+([Ee][+-]?\d+)[FfDd]?|\d+([Ee][
                                                  //| +-]?\d+)?[FfDd])(.*)
  val CharacterLiteral = """(?s)('.'|'\\[nbtfr'"\\]'|'\\[0-7]+'|'\\u[0-9a-fA-F]+')(.*)""".r
                                                  //> CharacterLiteral  : scala.util.matching.Regex = (?s)('.'|'\\[nbtfr'"\\]'|'\
                                                  //| \[0-7]+'|'\\u[0-9a-fA-F]+')(.*)
  val Id = id.r                                   //> Id  : scala.util.matching.Regex = (?s)(([A-Z$_][a-zA-Z$_0-9]*(_[!#$%&*+-/:<
                                                  //| =>?@\^|~]+)?|[a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%&*+-/:<=>?@
                                                  //| \^|~]+)|`[^`]+`)(.*)
  val Delimiter = """(?s)([;,.()\[\]{}])(.*)""".r //> Delimiter  : scala.util.matching.Regex = (?s)([;,.()\[\]{}])(.*)
  
  //val StringLiteral = "(\"\"\".??\"\"\"|\"((\\\"|[^\"])*\"))(.*)".r
  val StringLiteral1 = "(?s)(\\\"\\\"\\\".*?\\\"\\\"\\\")(.*)".r
                                                  //> StringLiteral1  : scala.util.matching.Regex = (?s)(\"\"\".*?\"\"\")(.*)
//  val StringLiteral2 = """(?s)(".*?")(.*)""".r
  val StringLiteral2 = "(?s)(\"(\\\\.|.*?)*\")(.*)".r
                                                  //> StringLiteral2  : scala.util.matching.Regex = (?s)("(\\.|.*?)*")(.*)
      
      
  val SymbolLiteral = "(?s)'%s(.*)".format(plainid).r
                                                  //> SymbolLiteral  : scala.util.matching.Regex = (?s)'([A-Z$_][a-zA-Z$_0-9]*(_[
                                                  //| !#$%&*+-/:<=>?@\^|~]+)?|[a-z][a-zA-Z$_0-9]*(_[!#$%&*+-/:<=>?@\^|~]+)?|[!#$%
                                                  //| &*+-/:<=>?@\^|~]+)(.*)
//  val SingleLineComment = "(?s)(//($|[^\\n]*))(.*)".r

  def nextToken(text: String): (Token, String) = {
    if (text startsWith "//") {
      val endOfComment = text indexOf "\n"
      if (endOfComment == -1) ((CommentType, text), "")
      else ((CommentType, text.substring(0, endOfComment)), text.substring(endOfComment))
    } else if (text startsWith "/*") {
      val endOfComment = text indexOf "*/"
      if (endOfComment == -1) ((ErrorType, text), "")
      else ((CommentType, text.substring(0, endOfComment + 2)), text.substring(endOfComment + 2))
    } else
      text match {
        case "" => Eof
        case SkipWhitespace(rest) => nextToken(rest)
        case FloatingPointLiteral(token, _, _, _, _, rest) => ((FloatingPointType, token), rest)
        case IntegerLiteral(token, _, _, _, rest) => ((IntegerType, token), rest)
        case CharacterLiteral(token, rest) => ((CharacterType, token), rest)
        //    case SingleLineComment(token, _, rest) => ((CommentType, token), rest)
        case Id(token, _, _, _, rest) =>
          if (keywords contains token) ((KeywordType, token), rest)
          else ((IdType, token), rest)
        //     case Delimiter(token, rest) => ((KeywordType, token), rest)
        case SymbolLiteral(token, _, _, rest) => ((SymbolType, token), rest)
        case StringLiteral1(token, rest)      => ((StringType, token), rest)
        case StringLiteral2(token, _, rest)   => ((StringType, token), rest)
        case rest => {
          val firstChar = rest(0)
          if ("()[]{};,." contains firstChar) ((DelimiterType, firstChar.toString), rest.substring(1))
          else if ("\u21D2\u2190" contains firstChar) ((KeywordType, firstChar.toString), rest.substring(1))
          else ((ErrorType, rest), rest.substring(1))
        }
      }
  }                                               //> nextToken: (text: String)((tokenizer.Tokenizer.TokenType, String), String)

  def getAllTokens(source: String): List[Token] = {
    @tailrec def accumulateTokens(acc: List[Token], source: String): List[Token] =
      if (source == "") acc
      else {
        val (token, rest) = nextToken(source)
        accumulateTokens(token :: acc, rest)
      }
    accumulateTokens(List(), source)
  }                                               //> getAllTokens: (source: String)List[(tokenizer.Tokenizer.TokenType, String)]
                                                  //| 

  def showInstructions = {
    val instructions =
      """This program counts tokens in files.
      
      When you close this dialog, an open file dialog will appear,
      allowing you to choose a file. Once you do so, another open
      file dialog will appear, then another, and another. To end
      the series, click Cancel.
      
      The program will then display the combined number of tokens
      each type."""
      Dialog.showMessage(null, instructions, "Instructions")
  }                                               //> showInstructions: => Unit

  def countTokensInFiles: Counts = {
    def extendCounts(file: File, counts: Counts): Counts = {
      val source = Source.fromFile(file toString, "UTF-8")
      val contents = source mkString
      val newTokens = getAllTokens(contents)
      for (token <- newTokens) counts(token._1) = 1 + counts.getOrElse(token._1, 0)
      counts
    }
    var counts: Counts = scala.collection.mutable.Map()
    var fileOption = chooseFile()
    while (fileOption != None) {
      val Some(file) = fileOption
      counts = extendCounts(file, counts)
      fileOption = chooseFile()
    }
    counts
  }                                               //> countTokensInFiles: => tokenizer.Tokenizer.Counts
  
  def main(args: Array[String]): Unit = {
    showInstructions
    val counts = countTokensInFiles
    printCounts(counts)
  }                                               //> main: (args: Array[String])Unit


  def printCounts(counts: Counts) {
    println("---------- Counts ----------")
    for ((key, value) <- counts) println("%5d %s".format(value, key.toString))
    val commentCount = counts getOrElse(CommentType, 0)
    val howMany = counts.foldLeft (0) ((x, y) => x + y._2) - commentCount
    println("-----\n%5d TOTAL TOKENS (not counting the %d comments)".format(howMany, commentCount))
  }                                               //> printCounts: (counts: tokenizer.Tokenizer.Counts)Unit

  /** Ask the user to choose an input file.
    * @return The file (or `None`).
    */
  def chooseFile(): Option[File] = {
    val chooser = new FileChooser()
    import FileChooser.Result._
    val theFileResult = chooser.showOpenDialog(null)
    theFileResult match {
      case Approve =>
        Some(chooser.selectedFile)
      case Cancel => None
      case Error =>
        println("Error: Unable to open file.")
        None
    }
  }                                               //> chooseFile: ()Option[java.io.File]
}