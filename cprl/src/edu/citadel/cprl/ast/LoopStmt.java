package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.Type;

import java.util.*;
import java.io.IOException;


/**
 * The abstract syntax tree node for a loop statement.
 */
public class LoopStmt extends Statement {
    private Expression whileExpr;
    private List<Statement> statements;

    // labels used during code generation
    private String L1;    // label for start of loop
    private String L2;    // label for end of loop


    /**
     * Default constructor.  Construct a loop statement with a null "while"
     * expression and an empty list of statements for the loop body.
     */
    public LoopStmt() {
        this.whileExpr = null;
        this.statements = new ArrayList<>();
    }


    /**
     * Set the while expression for this loop statement.
     */
    public void setWhileExpr(Expression whileExpr) {
        this.whileExpr = whileExpr;
    }


    /**
     * Returns the list of statements for the body of this loop statement.
     */
    public List<Statement> getStatements() {
        return statements;
    }


    /**
     * Set the list of statements for the body of this loop statement.
     */
    public void setStatements(List<Statement> statements) {
        this.statements = statements;
    }


    /**
     * Returns the label for the end of the loop statement.
     */
    public String getExitLabel() {
        return L2;
    }


    @Override
    public void checkConstraints() {
        try {

            if (whileExpr != null && whileExpr.getType() != Type.Boolean) {
                throw error(whileExpr.getPosition(),
                        "While expression should have type Boolean.");
            }

        } catch (ConstraintException ce) {
            ErrorHandler.getInstance().reportError(ce);
        }
    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
