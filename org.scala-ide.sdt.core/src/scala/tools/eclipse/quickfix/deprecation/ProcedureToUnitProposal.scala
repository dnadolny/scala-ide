package scala.tools.eclipse.quickfix.deprecation

import scala.tools.eclipse.logging.HasLogger
import scala.tools.eclipse.quickfix.BasicCompletionProposal
import scala.tools.eclipse.util.parsing.ScalariformParser
import scala.tools.eclipse.util.parsing.ScalariformUtils

import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.Position
import org.eclipse.text.edits.ReplaceEdit

import scalariform.parser.FunDefOrDcl

case class ProcedureToUnitProposal(pos: Position) extends BasicCompletionProposal(95, "Change procedure to ': Unit' syntax") with HasLogger {
  override def apply(document: IDocument): Unit = try {
    val source = document.get
    val Some((scalariformCU, _)) = ScalariformParser.safeParse(source)
    val Some(funDef) = ScalariformUtils.toStream(scalariformCU).collectFirst {
      case funDef: FunDefOrDcl if funDef.tokens.headOption.map(_.offset > pos.offset).getOrElse(false) => funDef //TODO: this will need to change when the compiler warning gives us the offset to the function
    }

    val offset = funDef.nameToken.offset + funDef.nameToken.text.length
    val returnUnitText = funDef.funBodyOpt match {
      case Some(_) => ": Unit ="
      case None => ": Unit"
    }
    new ReplaceEdit(offset, 0, returnUnitText).apply(document)
  } catch {
    case e: Throwable => logger.error("Problem changing procedure to Unit", e)
  }
}

/*
Examples:

def someMethod
becomes
def someMethod: Unit

def someMethod2 {}
becomes
def someMethod2: Unit = {}

def `some method 3`
becomes
def `some method 3`: Unit
*/