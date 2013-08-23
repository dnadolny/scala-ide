package scala.tools.eclipse.quickfix.deprecation

import scala.tools.eclipse.logging.HasLogger
import scala.tools.eclipse.quickfix.BasicCompletionProposal
import scala.tools.eclipse.util.parsing.ScalariformParser
import scala.tools.eclipse.util.parsing.ScalariformUtils

import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.Position
import org.eclipse.text.edits.ReplaceEdit

import scalariform.lexer._
import scalariform.parser._

case class ViewBoundToImplicitProposal(pos: Position) extends BasicCompletionProposal(95, "Change view bounds to implicit evidence") with HasLogger {
  override def apply(document: IDocument): Unit = try {
    val source = document.get
    val Some((scalariformCU, _)) = ScalariformParser.safeParse(source)
    val Some(funDef) = ScalariformUtils.toStream(scalariformCU).collectFirst {
      case funDef: FunDefOrDcl if funDef.tokens.headOption.map(_.offset > pos.offset).getOrElse(false) => funDef //TODO: this will need to change when the compiler warning gives us the offset to the function
    }
    val typeParamClause = funDef.typeParamClauseOpt.get
    val viewBoundTypeParams = collectViewBoundTypeParams(typeParamClause.contents)
    val existingNames = funDef.paramClauses.paramClausesAndNewlines.flatMap(_._1.tokens).collect{case Token(Tokens.VARID, name, _, _) => name}
    val evidence = createEvidence(source, viewBoundTypeParams, existingNames.toSet)

    val edits = evidenceInsertionEdit(evidence, funDef) :: removeViewBoundEdits(viewBoundTypeParams)
    for (edit <- edits) {
      edit.apply(document)
    }
  } catch {
    case e: Throwable => logger.error("Problem replacing view bounds", e)
  }
  case class ViewBoundTypeParam(name: String, startOffset: Int, viewBoundOffset: Int, endOffset: Int)

  private def collectViewBoundTypeParams(typeElements: List[TypeElement]) = typeElements collect {
    case typeParam @ TypeParam(_ :: GeneralTokens(List(Token(Tokens.VIEWBOUND, _, viewBoundOffset, _))) :: _) =>
      val typeParamName = typeParam.tokens.head
      val startOffset = typeParamName.offset + typeParamName.text.length
      val lastToken = typeParam.tokens.maxBy(_.offset)
      val endOffset = lastToken.offset + lastToken.text.length
      ViewBoundTypeParam(typeParamName.text, startOffset, viewBoundOffset, endOffset)
  }

  private def createEvidence(source: String, viewBoundTypeParams: List[ViewBoundTypeParam], existingNames: Set[String]) = {
    val freshNames = Stream.from(1).map("evidence$" + _).filterNot(existingNames.contains)
    val evidences = for ((typeParam, freshEvidenceName) <- viewBoundTypeParams.zip(freshNames)) yield {
      val bounds = source.substring(typeParam.viewBoundOffset + "%>".length, typeParam.endOffset).trim //the part after %>
      s"$freshEvidenceName: ${typeParam.name} => $bounds"
    }
    evidences.mkString(", ")
  }

  private def evidenceInsertionEdit(evidence: String, funDef: FunDefOrDcl) = {
    val (evidenceToInsert, offset) = funDef.paramClauses.paramClausesAndNewlines.map(_._1).reverse match {
      case Nil =>
        //no parameter list, so we'll add it after end of the type param square bracket
        val last = funDef.typeParamClauseOpt.get.tokens.maxBy(_.offset)
        ("(implicit " + evidence + ")", last.offset + last.text.length)
      case ParamClause(_, Some(Token(Tokens.IMPLICIT, _, _, _)), _, _, rparen) :: _ =>
        //we must add it to the existing implicit parameter list
        (", " + evidence, rparen.offset)
      case ParamClause(_, _, _, _, rparen) :: _ =>
        //add it to a new implicit parameter list, immediately after the right bracket of the last parameter list
        ("(implicit " + evidence + ")", rparen.offset + 1)
    }

    new ReplaceEdit(offset, 0, evidenceToInsert)
  }

  private def removeViewBoundEdits(viewBoundTypeParams: List[ViewBoundTypeParam]) = {
    var numDeletedCharacters = 0
    for (typeParam <- viewBoundTypeParams) yield {
      val length = typeParam.endOffset - typeParam.startOffset
      val edit = new ReplaceEdit(typeParam.startOffset - numDeletedCharacters, length, "")
      numDeletedCharacters += length
      edit
    }
  }
}

/*
Examples:

def someMethod[T <% Int, U <: Int, V <% Double with Int](t: T, u: U, v: V) = ???
becomes
def someMethod[T, U <: Int, V](t: T, u: U, v: V)(implicit evidence$1: T => Int, evidence$2: V => Double with Int) = ???

def someMethod2[T <% Int, U <: Int, V <% Double with Comparable[V]](evidence$2: Int)(implicit n: Int) = ???
becomes
def someMethod2[T, U <: Int, V](evidence$2: Int)(implicit n: Int, evidence$1: T => Int, evidence$3: V => Double with Comparable[V]) = ???

def someMethod3[T <% Int, U <: Int, V <% Double with Int] = ???
becomes
def someMethod3[T, U <: Int, V](implicit evidence$1: T => Int, evidence$2: V => Double with Int) = ???
*/