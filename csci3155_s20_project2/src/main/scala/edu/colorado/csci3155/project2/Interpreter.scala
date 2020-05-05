package edu.colorado.csci3155.project2

/*
   A Lettuce interpreter with evalExpr function that has missing cases to be handled. See TODOs below.

    sealed trait Value
    case class NumValue(f: Double) extends Value
    case class FigValue(c: MyCanvas) extends Value
    case class Closure(x: String, e: Expr, env: Environment) extends Value
    case class BoolValue(b: Boolean) extends Value
 */
object Interpreter {
    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => {
            val v = {
                val eval = l match{
                    case Const(d) => evalExpr(l, env)
                    case Ident(s) => evalExpr(l, env)
                    case _ => throw new IllegalArgumentException(s"You gave me a bad measurement for a line: $l")
                }
                val ret = eval match{
                    case NumValue(f) => f
                }
                ret
            }
            // create the line as a polygon then just box it up
            val line = new Polygon(List((0.0,0.0), (v,0.0)))
            val canvas = new MyCanvas(List(line))
            FigValue(canvas)

        } //TODO: Handle a line object

        case EquiTriangle(sideLength) => {
          val v = {
            val eval = sideLength match {
              // ultimately these will return NumValue(_) , you want to extract this value
              case Const(d) => evalExpr(sideLength, env)
              case Ident(f) => evalExpr(sideLength, env)
              case _ => throw new IllegalArgumentException(s"You gave me a bad expression for a rectangle sidelength: $sideLength")
            }
            val ret = eval match{
              case NumValue(f) => f
            }
            ret
          }
          val expr1 = v / 2
          val expr2 = ((scala.math.sqrt(3) * v)/2)
          val vertices = List ((0.0,0.0), (v, 0.0),(expr1, expr2) )

          val triangle = new Polygon(vertices)
          val canvas = new MyCanvas(List(triangle))
          FigValue(canvas)
        } // TODO: Handle Equilateral Triangle

        // TODO: Handle square given the side length
            /*
                 All of these need to return a value, the shapes have to return the shape they are inside of canvas
                    so you need to ..
                        1. Create the polygon
                        2. encapsulate it into a canvas
                        3. encapsulate that into the figValue type
                        4 return that figvalue.
                    Rectangle => FigValue(Canvas(Polygon((0,0), (0,v) , (v,v) , (v,0)))
             */
        case Rectangle(sideLength) => {
//            // side length is in the form: Const(_)  , case class NumValue(f: Double) extends Value <-- want to unpack this
            val v = {
                val eval = sideLength match {
                    // ultimately these will return NumValue(_) , you want to extract this value
                    case Const(d) => evalExpr(sideLength, env)
                    case Ident(f) => evalExpr(sideLength, env)
                    case _ => throw new IllegalArgumentException(s"You gave me a bad expression for a rectangle sidelength: $sideLength")
                }
                val ret = eval match{
                    case NumValue(f) => f
                }
                ret
            }
            val vertices = List( (0.0,0.0), (0.0,v), (v,v), (v,0.0) )
            val rectangle = new Polygon(vertices)
            // now put it into a canvas and box it up into a figValue
            val canvas = new MyCanvas(List(rectangle))

            FigValue(canvas)
        }

            /*
                   construction of a circle is just setting center equal to the (rad,rad) and radius is given also as rad, in the form Const(_) do the same thing
             */
        case Circle(rad) => {
            val v = {
                val eval = rad match{
                    case Const(f) => evalExpr(rad,env)
                    case Ident(s) => evalExpr(rad, env)
                    case _ => throw new IllegalArgumentException(s"You gave me an illegal measurement for creating a circle: $rad")
                }
                val ret = eval match{
                    case NumValue(f) => f
                }
                ret
            }

            val circle = new MyCircle((v,v), v)
            val canvas = new MyCanvas(List(circle))
            FigValue(canvas)
        } //TODO: Handle circle

          /*
              you are either going to be adding two figure or adding two numbers, otherwise throw an error
              adding two figures =>
                overlap the two figures
              adding two numbers =>
                add the two numbers
           */
        case Plus (e1, e2) => {
            val topUnboxed = evalExpr(e1, env)
            val bottomUnboxed = evalExpr(e2, env)

          // its either a FigValue or a NumValue
          val plusEval = topUnboxed match {
            case FigValue(c) => {
              val ret = bottomUnboxed match {
                    // overlap them
                case FigValue(c2) => {
                  FigValue(c2.overlap(c))
                }
                case _ => {
                  throw new IllegalArgumentException(s"Cannot perform plus operation")
                }
              }
              ret
          }
            case NumValue(d) => {
              val ret = bottomUnboxed match {
                case NumValue(d2) => {NumValue(d + d2)}
                case _ => {throw new IllegalArgumentException(s"you cannot perform plus operation")}
              }
              ret
            }
            case _ => throw new IllegalArgumentException(s"You gave me bad inputs for plus operation")
          }
          plusEval
          // you now have them both in either figValue or a NumValue
        } // TODO: Handle addition of numbers or figures

        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)

          /*
            you either mulitply two numbers or multiply two figures
              multiplying two figures =>
                place figure two to the right of figure one
              multiplying two numbers =>
                normal multiplication arithmetic
           */
        case Mult(e1, e2) => {
          val topUnboxed = evalExpr(e1, env)
          val bottomUnboxed = evalExpr(e2, env)

          val multEval = topUnboxed match {
            case FigValue(c) => {
              val ret = bottomUnboxed match{
                case FigValue(c2) => {
                    FigValue(c.placeRight(c2))
                }
                case _ => throw new IllegalArgumentException(s"You gave me a bad input for multiplication")
              }
              ret
            }
            case NumValue(d) => {
              val ret = bottomUnboxed match {
                case NumValue(d2) => {NumValue(d * d2)}
                case _ => throw new IllegalArgumentException(s"You gave me a bad input for multiplication")
              }
              ret
            }
            case _ => {throw new IllegalArgumentException(s"You gave me bad expressions for multiplication")}
          }
          multEval
        } // TODO: Handle multiplication of numbers or figures
            /*
                need to handle division between numbers and division between shape values , handle it with case matching as always
                3 things.
                    number / number => normal division arithmetic
                    figure1 / figure2 => place figure1 over figure2
                    Figure(f1) / number(d) => rotate f1 around (0,0) d radians
                    _ => throw an error

                    need to eval the expressions given, unbox them, perform whatever you need to do, then box the back up as a figvalue
             */
        case Div(e1, e2) => {
            val top = evalExpr(e1, env)
            val bottom = evalExpr(e2, env)

            val top_unboxed = {
                val ret = top match{
                    case FigValue(c) => FigValue(c)   // returns you the canvas
                    case NumValue(d) => NumValue(d)   // returns you the numValue
                    case _ => { throw new IllegalArgumentException(s"You gave me a bad expression to evaluate division: $top")}
                }
                ret
            }
            val bottom_unboxed = {
                val ret = bottom match{
                    case FigValue(c) => FigValue(c) // returns you the canvas
                    case NumValue(d) => NumValue(d) // returns you the numValue
                    case _ => throw new IllegalArgumentException(s"You gave me a bad expression to evaluate divison: $bottom")
                }
                ret
            }

            // now you want to apply the division rules accordingly
            val divEval = top_unboxed match {
              case FigValue(c) => {
                val ret = bottom_unboxed match{
                  // Another canvas
                  case FigValue(c2) => {
                    FigValue(c2.placeTop(c))
                  }
                  // A num Value
                  case NumValue(d) => {
                    FigValue(c.rotate(d))
                  }
                }
                ret
              }
                // dividing a number with a number, cannot have num / fig
              case NumValue(d) => {
                val ret = bottom_unboxed match {

                  case NumValue(d2) => {
                    ValueOps.divide(NumValue(d), NumValue(d2))
                  }
                  // anything else is an error
                  case _ => throw new IllegalArgumentException(s"Cannot perform division betewen $top_unboxed and $bottom_unboxed")
                }
                ret
              }
            }
          divEval
        }// TODO: Handle division

        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => {
          Closure(x,e, env)

        } //TODO: Handle function definitions

          /*
            case LetRec(rfun, x, fExpr, bExpr) => {
            val env2 = ExtendRec(rfun, x, fExpr, env)
            evalExpr(bExpr, env2)
            extend the environment then just pass it on again
        }

        <functionName> , <formalParameter>, <functionBody> , in <otherFuncBody>

        case class ExtendREC(f: String, x: String, e: Expr, env: Environment) extends Environment {
           */
        case LetRec(f, x, e1, e2) => {
          val extendedEnv = ExtendREC(f,x,e1, env)
          evalExpr(e2, extendedEnv)

        } // TODO: Handle recursive functions -- look at Environment.scala

          // single argument function calls
        case FunCall(fCallExpr, arg) => {
          val closureReturned = evalExpr(fCallExpr, env)
          val retValue = closureReturned match {
            case Closure(paramName,funcBody,envOld) => {
              val okParam = evalExpr(arg, env)  // if this is a bad argument, the program will bail and throw a exception
              val extendedEnvironment = Extend(paramName, okParam, envOld)
              evalExpr(funcBody, extendedEnvironment)
            }
            case _ => throw new IllegalArgumentException(s"Cannot evaluate a function that does not exist")
          }
          retValue
        } // TODO: Handle function calls
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
/*
       pretty much just gotta implement everything but let rec for the next tests
 */