package edu.colorado.csci3155.project1

object StackMachineCompiler {
    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */

    /*
        You are given an an expression, you need to turn that expression into ListOfInstructions.
            when you parse it you need to work from left to right, outside in

        this will function similarly to how you did it in assignment4, just keep returning a list of appended lists
    */

    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = e match{


// UNARY EXPRESSIONS
        // translating a const expression to machine instruction, Const(e) => PushI(e)
        case Const(v) => {
            List(PushI(v))
        }

        // have to remember to evaluate the inner expression too
        case Log(e) => {
            val innerExpression_Instruction = compileToStackMachineCode(e)
            val Log_instructions = List(LogI)

            innerExpression_Instruction ++ Log_instructions
        }

        case Exp(e)  => {
            val innerExpression_Instruction = compileToStackMachineCode(e)
            val Exp_instructions = List(ExpI)

            innerExpression_Instruction ++ Exp_instructions
        }

        case Cosine(e1) => {
            val innerExpression_Instruction = compileToStackMachineCode(e1)
            val cosine_Expression = List(CosI)

            innerExpression_Instruction ++ cosine_Expression
        }

        case Sine(e1) => {
            val innerExpression_Instruction = compileToStackMachineCode(e1)
            val sine_expression = List(SinI)

            innerExpression_Instruction ++ sine_expression
        }


        // BINARY EXPRESSIONS
        // Addition Rule, evaluate e1, e2 then append the AddI to the end , use ++ for list concatination, this will be done recursively
        // the rule is that if we have instruction of e1 and e2 then instruction of plus =>  List(e1_ins, e2,ins, AddI)
        // any error of bad input will be caught in the next recursive call , case _ .
        case Plus(e1,e2) => {
            val e1_instruction = compileToStackMachineCode(e1)
            val e2_instruction = compileToStackMachineCode(e2)
            val addInstruction = List(AddI)
            println("E1 TRANSLATED: ", e1_instruction)  // this returns PushI(1.0)
            println("E2 TRANSLATED: ", e2_instruction)  // this returns PushI(2.5)

            e1_instruction ++ e2_instruction ++ addInstruction
        }

        // this should be almost identical to how the add works
        case Minus(e1, e2) =>  {
            val e1_instruction = compileToStackMachineCode(e1)
            val e2_instruction = compileToStackMachineCode(e2)
            val SubI_Instruction = List(SubI)
            println("E1 TRANSLATED: ", e1_instruction)  // this returns PushI(1.0)
            println("E2 TRANSLATED: ", e2_instruction)  // this returns PushI(2.5)

            e1_instruction ++ e2_instruction ++ SubI_Instruction
        }

        case Mult(e1, e2) => {
            val e1_instruction = compileToStackMachineCode(e1)
            val e2_instruction = compileToStackMachineCode(e2)
            val Mult_Instruction = List(MultI)

            println("E1 TRANSLATED: ", e1_instruction)  // this returns PushI(1.0)
            println("E2 TRANSLATED: ", e2_instruction)  // this returns PushI(2.5)

            e1_instruction ++ e2_instruction ++ Mult_Instruction
        }


        case Div(e1, e2) => {
            val e1_instruction = compileToStackMachineCode(e1)
            val e2_instruction = compileToStackMachineCode(e2)
            val Div_Instruction = List(DivI)

            println("E1 TRANSLATED: ", e1_instruction)  // this returns PushI(1.0)
            println("E2 TRANSLATED: ", e2_instruction)  // this returns PushI(2.5)

            e1_instruction ++ e2_instruction ++ Div_Instruction
        }

        case _ => {
            throw new IllegalArgumentException(s"ERROR: You gave me an illegal expression ${e}")
        }


    }

}
