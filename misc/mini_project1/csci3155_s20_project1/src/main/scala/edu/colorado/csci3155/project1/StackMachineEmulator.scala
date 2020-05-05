package edu.colorado.csci3155.project1

sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] = ins match{
        /*
        PushI(d): push the number d onto the stack
        PopI: pop off the top element of the stack

            - throw an exception if the stack is empty
        AddI: pop two numbers off of the stack, then add them, then push the result back onto the stack

            - throw an exception if stack is empty during any of the pop operations
        SubI: pop two numbers off of the stack

                - let the first number of the popped be v1 and the second number be equal to v2, subtract them as v2 - v1 (this order is very important) and push the result back on to the stack. Throw an exception if the stack is empty during any of the pop operations
        MultI: pop two numbers off of the stack, multiply them, then push the result back onto the stack. Throw an exception if the stack is empty during any of these operations
        DivI: pop two numbers from the stack, let the first number popped be v1 and the second number be v2, divide them as v2 / v1 (this order is very important) and push the result back to the stack. Throw and exception if the stack is empty during any of the pop operations.
        Log: pop one number from the stack, compute its log if positive and push the result back onto the stack. Throw an exception if the stack is empty during any of the pop operations

        SineI/CosineI: pop one number from the stack, compute its sin/cos respectively, and push the result back onto the stack. Throw and exception if the stack is empty during any of the pop operations
         */

        /* When you push something onto the stack, you are just adding the element to a list so merge and return a new list
            IMPORTANT: MAKE SURE YOU ADD THE ELEMENT AS THE VERY FIRST ONE NOT THE NEXT ONE
         */
        case PushI(d: Double) => {
            val elt = List(d)   // create the new item so you can merge it to the stack
            val newStack = elt ::: stack       // now the stack holds the new added element
            println("New stack after pushing: ", newStack)
            newStack
            //println(s"You have not yet implemented ${ins}")
            //List(0.0)
        }
        // lets see, this will really only be used for when you are going to conduct operations
        // so you would actually just want to pop off the top value of the list and return it as a new single list so it can be accessed
        // as v1, v2 as needed.
        case PopI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }else{
                // think think think, how will you pop off the stack, while at the same time, pass access to the popped value
                val poppedStack = stack.slice(1,stack.size)
                println("poppedStack: ", poppedStack)
                poppedStack
            }
        }

        // ultiamtely you will want to return the new stack after popping and such so you will just keep creating new list
        // after you pop elements
        case AddI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val stackAfterv1Pop = emulateSingleInstruction(stack, PopI)


            if (stackAfterv1Pop.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v2 = stackAfterv1Pop(0)
            val finalStack = emulateSingleInstruction(stackAfterv1Pop,PopI)  // upto this point you have popped prev 2 items off of the stack

            val Sum = v1 + v2
            val StackAfterPushingSum = emulateSingleInstruction(finalStack, PushI(Sum))
            StackAfterPushingSum
        }
        case SubI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val stackAfterv1Pop = emulateSingleInstruction(stack, PopI)

            if (stackAfterv1Pop.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v2 = stackAfterv1Pop(0)
            val finalStack = emulateSingleInstruction(stackAfterv1Pop,PopI)  // upto this point you have popped prev 2 items off of the stack

            val Diff = v2 - v1
            val StackAfterPushingDiff = emulateSingleInstruction(finalStack, PushI(Diff))
            StackAfterPushingDiff
        }
        case MultI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val stackAfterv1Pop = emulateSingleInstruction(stack, PopI)

            if (stackAfterv1Pop.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v2 = stackAfterv1Pop(0)
            val finalStack = emulateSingleInstruction(stackAfterv1Pop,PopI)  // upto this point you have popped prev 2 items off of the stack

            val product = v1 * v2
            val StackAfterPushingProduct = emulateSingleInstruction(finalStack, PushI(product))
            return StackAfterPushingProduct
        }
        case DivI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val stackAfterv1Pop = emulateSingleInstruction(stack, PopI)

            if (stackAfterv1Pop.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v2 = stackAfterv1Pop(0)
            val finalStack = emulateSingleInstruction(stackAfterv1Pop,PopI)  // upto this point you have popped prev 2 items off of the stack

            // edge case, check divide by zero
            if (v1 == 0){
                throw new IllegalArgumentException(s"ERROR: Cannot divide by zero.")
            }
            println("v2: ", v2)
            println("v1: ", v1)

            val quotient = v2 / v1
            val StackAfterPushingQuotient = emulateSingleInstruction(finalStack, PushI(quotient))
            return StackAfterPushingQuotient
//            println(s"You have not yet implemented ${ins}")
//            List(0.0)
        }

// UNARY OPERATIONS
        // the only edge case you are going to need to catch for log is n < 0
        case LogI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }
            val top = stack(0)
            val newStackAfterPop = emulateSingleInstruction(stack, PopI)

            // check if n < 0
            if (top < 0){
                throw new IllegalArgumentException(s"Cannot take log of ${top}")
            }
            else{
                val pushMe = scala.math.log(top)
                val finalStack = emulateSingleInstruction(newStackAfterPop, PushI(pushMe))
                finalStack
            }
        }

        // shouldnt be any edge cases to catch here, just compute its exponential, negative values should be fine?
        case ExpI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }
            val v1 = stack(0)
            val popStack = emulateSingleInstruction(stack, PopI)
            val expVal = scala.math.exp(v1)
            val finalStack = emulateSingleInstruction(popStack, PushI(expVal))
            finalStack
        }

        case SinI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val popStack = emulateSingleInstruction(stack, PopI)
            val sin_val = scala.math.sin(v1)
            val finalStack = emulateSingleInstruction(popStack, PushI(sin_val))
            finalStack
        }

        case CosI => {
            if (stack.isEmpty){
                throw new IllegalArgumentException(s"stack is empty")
            }

            val v1 = stack(0)
            val popStack = emulateSingleInstruction(stack, PopI)
            val cos_val = scala.math.cos(v1)
            val finalStack = emulateSingleInstruction(popStack, PushI(cos_val))
            finalStack
        }

        case _ => {
            throw new IllegalArgumentException(s"You gave me an illegal argument")
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.

       function returns an accumulated List that is generated / manipulated by emulateSingleInstruction() so
       ultimatley, that should leave the answer lying at the top of the list after the stack has finished executing, this should be your return
       value
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double ={
        // emulateSingleInstruction() takes in Stack (its just a list) as the first parameter and the instruction list as the second
        // paremeter, it should return the state of Stack after execution of all instructions.
        val initialStack = List[Double]()
        val newStack = instructionList.foldLeft (initialStack) ( (initialStack, currInstruction) => emulateSingleInstruction(initialStack, currInstruction) )
        return newStack(0)
    }


}