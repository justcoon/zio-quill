package io.getquill

import io.getquill.ast._
import io.getquill.context.sql._
import io.getquill.context.sql.idiom._
import io.getquill.idiom.StatementInterpolator._
import io.getquill.idiom.{ Statement, StringToken, Token }

trait OracleDialect
  extends SqlIdiom
  with QuestionMarkBindVariables
  with ConcatSupport {

  override def emptySetContainsToken(field: Token) = StringToken("1 <> 1")

  override protected def limitOffsetToken(query: Statement)(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) =
    Tokenizer[(Option[Ast], Option[Ast])] {
      case (Some(limit), None)         => stmt"$query FETCH FIRST ${limit.token} ROWS ONLY"
      case (Some(limit), Some(offset)) => stmt"$query OFFSET ${offset.token} ROWS FETCH NEXT ${limit.token} ROWS ONLY"
      case (None, Some(offset))        => stmt"$query OFFSET ${offset.token} ROWS"
      case other                       => super.limitOffsetToken(query).token(other)
    }

  override implicit def operationTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Operation] =
    Tokenizer[Operation] {
      case BinaryOperation(a, NumericOperator.`%`, b) => stmt"MOD(${a.token}, ${b.token})"
      case other                                      => super.operationTokenizer.token(other)
    }

  override implicit def sourceTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[FromContext] = Tokenizer[FromContext] {
    case QueryContext(query, alias) => stmt"(${query.token}) ${strategy.default(alias).token}"
    case other                      => super.sourceTokenizer.token(other)
  }

  override protected def tokenizeColumn(strategy: NamingStrategy, column: String): String =
    tokenizeEscapeUnderscores(strategy, column)

  override protected def tokenizeTable(strategy: NamingStrategy, table: String): String =
    tokenizeEscapeUnderscores(strategy, table)

  override protected def tokenizeAlias(strategy: NamingStrategy, table: String): String =
    tokenizeEscapeUnderscores(strategy, table)

  private def tokenizeEscapeUnderscores(strategy: NamingStrategy, column: String): String =
    if (column.startsWith("_"))
      Escape.column(column)
    else
      strategy.column(column)

  override def defaultAutoGeneratedToken(field: Token) = stmt"($field) VALUES (DEFAULT)"

  override def prepareForProbing(string: String) = string
}

object OracleDialect extends OracleDialect