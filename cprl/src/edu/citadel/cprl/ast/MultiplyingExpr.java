package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.Symbol;
import edu.citadel.cprl.Token;
import edu.citadel.cprl.Type;

import java.io.IOException;


/**
 * The abstract syntax tree node for a multiplying expression.  A multiplying
 * expression is a binary expression where the operator is a multiplying operator
 * such as "*", "/", or "mod".  A simple example would be "5*x".
 */
public class MultiplyingExpr extends BinaryExpr {
    /**
     * Construct a multiplying expression with the operator ("*", "/", or "mod")
     * and the two operands.
     */
    public MultiplyingExpr(Expression leftOperand, Token operator, Expression rightOperand) {
        super(leftOperand, operator, rightOperand);
        assert operator.getSymbol().isMultiplyingOperator() :
                "MultiplyingExpr: operator is not a multiplying operator";
    }


    @Override
    public void checkConstraints() {
        try {
            Expression leftOperand = getLeftOperand();
            Expression rightOperand = getRightOperand();

            leftOperand.checkConstraints();
            rightOperand.checkConstraints();

            // can multiply only integers

            if (leftOperand.getType() != Type.Integer) {
                String errorMsg = "Left operand for expression should have type Integer.";
                throw error(leftOperand.getPosition(), errorMsg);
            }

            if (rightOperand.getType() != Type.Integer) {
                String errorMsg = "Right operand for expression should have type Integer.";
                throw error(rightOperand.getPosition(), errorMsg);
            }
        } catch (ConstraintException e) {
            ErrorHandler.getInstance().reportError(e);
        }

        setType(Type.Integer);
    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
