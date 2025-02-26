package evaluator

import com.typesafe.scalalogging.Logger
import evaluator.builtin.{PrintFn, fns}
import evaluator.objects.BooleanObject.{False, True}
import evaluator.objects._
import parser.ast.expressions._
import parser.ast.statements.{
  BagugaStatement,
  BlockStatement,
  ExpressionStatement,
  LetStatement,
  ReturnStatement
}
import parser.ast.{Expression, Node, Program, Statement}
import token.Token
import token.TokenType.BAGUGA

case class Evaluator(initialContext: String) {

  val logger: Logger = Logger(Evaluator.getClass)
  val environment: Environment = Environment(fns, initialContext)

  var error: Option[ErrorObject] = None

  def evaluate(
      node: Node,
      context: String = initialContext
  ): Option[Anything] = {
    node match {
      case node: Program => evalStatements(node.statements, context)
      case node: ExpressionStatement =>
        node.expression match {
          case Some(expression) => evaluate(expression, context)
          case _                => None
        }
      case node: LetStatement =>
        evaluate(node.value, context) match {
          case Some(e: ErrorObject) => Some(e)
          case None                 => None
          case Some(otherObject: Anything) =>
            environment.addObject(context, node.name.value, otherObject)
        }
      case node: BlockStatement  => evalBlockStatement(node, context)
      case node: BagugaStatement => evalBagugaStatement(node, context)
      case node: IntegerLiteral  => Some(IntegerObject(node.value))
      case node: StringLiteral   => Some(StringObject(node.value))
      case node: BooleanLiteral  => Some(BooleanObject.get(node.value))
      case node: ArrayLiteral =>
        val evaluatedValues = node.values.map(evaluate(_, context))
        if (evaluatedValues.exists(value =>
              value.isEmpty || value.exists(_.objectType == ObjectType.Error)
            )) {
          Some(
            ErrorObject(
              s"Failed to evaluate array ${node.string} on ${evaluatedValues
                .find(_.isEmpty)}"
            )
          )
        } else {
          Some(ArrayObject(evaluatedValues.flatten))
        }
      case node: FunctionLiteral =>
        Some(CustomFunctionObject(node.parameters, node.body))
      case node: CallExpression =>
        node.function match {
          case function: Identifier =>
            environment.getObject(context = context, variable = function.value) match {
              case Some(f: FunctionObject) =>
                evalFunction(f, node.arguments, context)
              case Some(otherObject: Anything) =>
                Some(
                  ErrorObject(
                    s"${otherObject.objectType.toString} doesn't allow calls"
                  )
                )
              case None =>
                Some(
                  ErrorObject(
                    s"${function.value} not found on context $context"
                  )
                )
            }
          case function: FunctionLiteral =>
            evaluateFunctionLiteral(function, node.arguments, context)
          case otherObject =>
            Some(
              ErrorObject(
                s"${otherObject.string} doesn't allow call expressions!"
              )
            )
        }
      case node: Identifier =>
        environment.getObject(context, node.value) match {
          case Some(objectType: Anything) => Some(objectType)
          case None =>
            Some(ErrorObject(s"Identifier '${node.value}' not found"))
        }
      case node: InfixExpression =>
        (evaluate(node.left, context), evaluate(node.right, context)) match {
          case (Some(left), Some(right)) =>
            evalInfixExpression(node.operator, left, right)
          case _ => None
        }
      case node: PrefixExpression =>
        evaluate(node.right, context) match {
          case Some(right) =>
            evalPrefixExpression(node.operator, right)
          case _ => None
        }
      case node: WhileExpression =>
        evalWhileExpression(node, context)
      case node: IfExpression =>
        evalIfExpression(node, context)
      case ReturnStatement(_, returnValue) =>
        evaluate(returnValue, context) match {
          case Some(value) => Some(ReturnValue(value))
          case _           => None
        }

      case _ => None
    }
  }

  def evalBagugaStatement(
      node: BagugaStatement,
      context: String
  ): Option[Anything] = {
    node.statements
      .collect {
        case b: BagugaStatement => b
      }
      .reverse
      .map { bagugaStatement: BagugaStatement =>
        evalBagugaStatement(bagugaStatement, context)
      }

    val bagugaBlockStatement =
      BlockStatement(Token(BAGUGA, "BAGUGA"), node.statements.filter {
        case _: BagugaStatement => false
        case _: Statement       => true
      })

    evalBlockStatement(bagugaBlockStatement, context)
  }

  def evaluateFunctionLiteral(
      f: FunctionLiteral,
      arguments: Seq[Expression],
      context: String
  ): Option[Anything] = {
    val localContext = s"${f.hashCode().toString}.$context"
    f.parameters.zip(arguments).map {
      case (identifier: Identifier, expression: Expression) =>
        environment.addObject(
          context = localContext,
          variable = identifier.value,
          value = evaluate(expression, context).getOrElse(
            ErrorObject(
              s"Error parsing argument ${identifier.value} on function call"
            )
          )
        )
    }
    evaluate(f.body, localContext) match {
      case Some(obj) =>
        environment.deleteContext(localContext)
        Some(obj)
      case None =>
        None
    }
  }

  def evalFunction(
      f: FunctionObject,
      arguments: Seq[Expression],
      context: String
  ): Option[Anything] = {
    val localContext = s"${f.hashCode().toString}.$context"
    f match {
      case f: CustomFunctionObject =>
        f.parameters.zip(arguments).map {
          case (identifier: Identifier, expression: Expression) =>
            environment.addObject(
              context = localContext,
              variable = identifier.value,
              value = evaluate(expression, context).getOrElse(
                ErrorObject(
                  s"Error parsing argument ${identifier.value} on function call"
                )
              )
            )
        }
        evaluate(f.body, localContext)
      case f: BuiltinFunctionObject =>
        f.executor(
          arguments.map(arg => evaluate(arg, context).getOrElse(NullObject))
        )
    }
  }

  def evalBangOperator(expression: Anything): Option[BooleanObject] = {
    expression match {
      case BooleanObject(value: Boolean)      => Some(BooleanObject(!value))
      case IntegerObject(value) if value == 0 => Some(True)
      case IntegerObject(_)                   => Some(False)
      case _                                  => Some(False)
    }
  }

  def evaluateCondition(
      condition: Expression,
      context: String
  ): Option[Anything] = {
    evaluate(condition, context) match {
      case Some(condition: BooleanObject)           => Some(condition)
      case Some(_: NullObjectConstructor)           => Some(False)
      case Some(s: StringObject) if s.value == ""   => Some(False)
      case Some(i: IntegerObject) if i.value == 0   => Some(False)
      case Some(a: ArrayObject) if a.values.isEmpty => Some(False)
      case Some(errorObject: ErrorObject)           => Some(errorObject)
      case Some(_)                                  => Some(True)
      case None                                     => None
    }
  }

  def evalWhileExpression(
      expression: WhileExpression,
      context: String
  ): Option[Anything] = {
    while (evaluateCondition(expression.condition, context) match {
             case Some(condition: BooleanObject) => condition.value
             case Some(e: ErrorObject)           => return Some(e)
             case None =>
               return Some(
                 ErrorObject(
                   s"While expression failed on parsing the condition ${expression.condition.string}"
                 )
               )
             case _ => false
           }) {
      evaluate(expression.consequence, context) match {
        case Some(e: ErrorObject) => return Some(e)
        case None =>
          return Some(
            ErrorObject(
              s"While expression failed on parsing the expression ${expression.consequence.string}"
            )
          )
        case _ =>
      }
    }
    Some(NullObject)
  }

  def evalIfExpression(
      expression: IfExpression,
      context: String
  ): Option[Anything] = {
    evaluate(expression.condition, context) match {
      case Some(condition: BooleanObject) =>
        if (condition.value) {
          evaluate(expression.consequence, context)
        } else {
          expression.alternative match {
            case Some(alternative) => evaluate(alternative, context)
            case _                 => Some(NullObject)
          }
        }
      case Some(_: NullObjectConstructor) =>
        expression.alternative match {
          case Some(alternative) => evaluate(alternative, context)
          case _                 => Some(NullObject)
        }
      case Some(value: IntegerObject) if value.value == 0 =>
        expression.alternative match {
          case Some(alternative) => evaluate(alternative, context)
          case _                 => Some(NullObject)
        }
      case Some(_) =>
        evaluate(expression.consequence, context)
      case _ => None
    }
  }

  def evalMinusOperator(expression: Anything): Option[Anything] = {
    expression match {
      case IntegerObject(value) => Some(IntegerObject(-value))
      case e: ErrorObject       => Some(e)
      case otherObject =>
        error = Some(
          ErrorObject(s"unknown operator: -${otherObject.objectType.toString}")
        )
        error
    }
  }

  def evalIntegerInfixExpression(
      operator: String,
      left: IntegerObject,
      right: IntegerObject
  ): Option[Anything] = {
    operator match {
      case "+"  => Some(IntegerObject(value = left.value + right.value))
      case "-"  => Some(IntegerObject(value = left.value - right.value))
      case "/"  => Some(IntegerObject(value = left.value / right.value))
      case "*"  => Some(IntegerObject(value = left.value * right.value))
      case "<"  => Some(BooleanObject(value = left.value < right.value))
      case ">"  => Some(BooleanObject(value = left.value > right.value))
      case "==" => Some(BooleanObject(value = left.value == right.value))
      case "!=" => Some(BooleanObject(value = left.value != right.value))
      case otherOperand: String =>
        Some(
          ErrorObject(
            s"Unsupported operand '$otherOperand' between ${left.objectType.toString} and ${right.objectType.toString}"
          )
        )
    }
  }

  def evalStringInfixExpression(
      operator: String,
      left: StringObject,
      right: StringObject
  ): Option[Anything] = {
    operator match {
      case "+"  => Some(StringObject(value = left.value ++ right.value))
      case "<"  => Some(BooleanObject(value = left.value < right.value))
      case ">"  => Some(BooleanObject(value = left.value > right.value))
      case "==" => Some(BooleanObject(value = left.value == right.value))
      case "!=" => Some(BooleanObject(value = left.value != right.value))
      case otherOperand: String =>
        Some(
          ErrorObject(
            s"Unsupported operand '$otherOperand' between ${left.objectType.toString} and ${right.objectType.toString}"
          )
        )
    }
  }

  def evalArrayInfixExpression(
      operator: String,
      left: ArrayObject,
      right: ArrayObject
  ): Option[Anything] = {
    operator match {
      case "+"  => Some(ArrayObject(values = left.values ++ right.values))
      case "==" => Some(BooleanObject(value = left.isEqual(right)))
      case "!=" => Some(BooleanObject(value = left.isEqual(right)))
      case otherOperand: String =>
        Some(
          ErrorObject(
            s"Unsupported operand '$otherOperand' between ${left.objectType.toString} and ${right.objectType.toString}"
          )
        )
    }
  }

  def evalInfixExpression(
      operator: String,
      left: Anything,
      right: Anything
  ): Option[Anything] = {
    (left, right) match {
      case (left: IntegerObject, right: IntegerObject) =>
        evalIntegerInfixExpression(operator, left, right)
      case (left: StringObject, right: StringObject) =>
        evalStringInfixExpression(operator, left, right)
      case (left: ArrayObject, right: ArrayObject) =>
        evalArrayInfixExpression(operator, left, right)
      case (e: ErrorObject, _) => Some(e)
      case (_, e: ErrorObject) => Some(e)
      case _ =>
        operator match {
          case "==" => Some(BooleanObject(value = left == right))
          case "!=" => Some(BooleanObject(value = left != right))
          case operator: String =>
            error = Some(
              ErrorObject(
                s"type mismatch: ${left.objectType.toString} $operator ${right.objectType}"
              )
            )
            error

        }
    }
  }

  def evalPrefixExpression(
      operator: String,
      right: Anything
  ): Option[Anything] = {
    operator match {
      case "!" => evalBangOperator(right)
      case "-" => evalMinusOperator(right)
      case _   => None
    }
  }

  def evalBlockStatement(
      blockStatement: BlockStatement,
      context: String
  ): Option[Anything] = {
    val statementsEvaluations =
      blockStatement.statements.map(evaluate(_, context))

    if (statementsEvaluations.isEmpty) {
      None
    } else {
      statementsEvaluations
        .reduce((a, b) =>
          (a, b) match {
            case (Some(e: ErrorObject), _)  => Some(e)
            case (_, Some(e: ErrorObject))  => Some(e)
            case (Some(rv: ReturnValue), _) => Some(rv)
            case (_, Some(b))               => Some(b)
            case _                          => None
          }
        )
    }

  }

  def evalStatements(
      statements: Seq[Statement],
      context: String
  ): Option[Anything] = {
    val statementsEvaluations = statements.map(evaluate(_, context))

    if (statementsEvaluations.isEmpty) {
      None
    } else {
      statementsEvaluations
        .reduce((a, b) =>
          (a, b) match {
            case (Some(e: ErrorObject), _)        => Some(e)
            case (_, Some(e: ErrorObject))        => Some(e)
            case (Some(rv: ReturnValue), Some(_)) => Some(rv)
            case (_, Some(b))                     => Some(b)
            case _                                => None
          }
        ) match {
        case Some(returnValue: ReturnValue) => Some(returnValue.value)
        case other                          => other
      }
    }
  }

}
