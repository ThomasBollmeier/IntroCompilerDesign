package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.Token;

import java.util.*;
import java.io.IOException;


/**
 * The abstract syntax tree node for a function declaration.
 */
public class FunctionDecl extends SubprogramDecl {
    /**
     * Construct a function declaration with its name (an identifier).
     */
    public FunctionDecl(Token funcId) {
        super(funcId);
    }


    /**
     * Computes the relative address of the function return value. <br>
     * Note: This method assumes that the relative addresses of all
     * formal parameters have been set.
     */
    public int getRelAddr() {
        int firstParamAddr = 0;

        List<ParameterDecl> params = getFormalParams();
        if (params.size() > 0) {
            ParameterDecl firstParamDecl = params.get(0);
            firstParamAddr = firstParamDecl.getRelAddr();
        }

        // the location for the return value is above the first parameter
        return firstParamAddr - getType().getSize();
    }


    @Override
    public void checkConstraints() {
// ...  Hint: See SubprogramDecl
    }


    /**
     * Returns true if the specified list of statements contains at least one
     * return statement.
     *
     * @param statements the list of statements to check for a return statement.  If
     *                   any of the statements in the list contains nested statements
     *                   (e.g., an if statement or a loop statement), then the nested
     *                   statements are also checked for a return statement.
     */
    private boolean hasReturnStmt(List<Statement> statements) {
        return false; // TODO
    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
