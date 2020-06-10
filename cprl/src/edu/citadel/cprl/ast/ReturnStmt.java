package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.compiler.Position;

import java.io.IOException;


/**
 * The abstract syntax tree node for a return statement.
 */
public class ReturnStmt extends Statement {
    private Expression returnExpr;    // may be null
    private SubprogramDecl subprogramDecl;

    // position of the return token (needed for error reporting)
    private Position returnPosition;


    /**
     * Construct a return statement with a reference to the enclosing subprogram
     * and the expression for the value being returned, which may be null.
     */
    public ReturnStmt(SubprogramDecl subprogramDecl, Expression returnExpr, Position returnPosition) {

        this.subprogramDecl = subprogramDecl;
        this.returnExpr = returnExpr;
        this.returnPosition = returnPosition;
    }


    @Override
    public void checkConstraints() {
        assert subprogramDecl != null : "Return statement must be nested within a subprogram.";

        try {

            if (subprogramDecl instanceof FunctionDecl) {
                if (returnExpr == null) {
                    throw error(returnPosition,
                            "A value must be returned from a function");
                }

                FunctionDecl functionDecl = (FunctionDecl) subprogramDecl;

                if (returnExpr.getType() != functionDecl.getType()) {
                    throw error(returnPosition,
                            "The returned type does not match the result type of the function");
                }

            } else {
                if (returnExpr != null) {
                    throw error(returnPosition,
                            "No value must be returned from a procedure");
                }
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
