package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.compiler.Position;

import java.io.IOException;


/**
 * The abstract syntax tree node for an assignment statement.
 */
public class AssignmentStmt extends Statement {
    private Variable variable;
    private Expression expr;

    // position of the assignment operator (needed for error reporting)
    private Position assignPosition;


    /**
     * Construct an assignment statement with a variable, an expression,
     * and the position of the assignment operator.
     *
     * @param variable       the variable on the left side of the assignment operator
     * @param expr           the expression on the right side of the assignment operator
     * @param assignPosition the position of the assignment operator (for error reporting)
     */
    public AssignmentStmt(Variable variable, Expression expr, Position assignPosition) {
        this.variable = variable;
        this.expr = expr;
        this.assignPosition = assignPosition;
    }


    @Override
    public void checkConstraints() {

        variable.checkConstraints();
        expr.checkConstraints();

        if (!variable.getType().equals(expr.getType())) {
            try {
                throw error(assignPosition,
                        "Types of left and right hand side do not match");
            } catch (ConstraintException e) {
                ErrorHandler.getInstance().reportError(e);
            }
        }

    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
