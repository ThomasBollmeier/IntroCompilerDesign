package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.compiler.Position;
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

        try {

            super.checkConstraints();

            for (ParameterDecl paramDecl : getFormalParams()) {
                if (paramDecl.isVarParam()) {
                    Position position = paramDecl.getPosition();
                    String errorMsg = "No var parameters are allowed in a function declaration";
                    throw error(position, errorMsg);
                }
            }

            if (!hasReturnStmt(getStatementPart().getStatements())) {
                Position position = getPosition();
                String errorMsg = "Function must have at least one return statement";
                throw error(position, errorMsg);
            }

        } catch (ConstraintException ce) {
            ErrorHandler.getInstance().reportError(ce);
        }

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

        for (Statement statement: statements) {
            if (statement instanceof ReturnStmt) {
                return true;
            } else if (statement instanceof IfStmt) {
                IfStmt ifStmt = (IfStmt) statement;
                if (hasReturnStmt(ifStmt.getThenStmts())) {
                    return true;
                }
                List<ElsifPart> elsifParts = ifStmt.getElsifParts();
                for (ElsifPart elsifPart: elsifParts) {
                    if (hasReturnStmt(elsifPart.getThenStmts())) {
                        return true;
                    }
                }
                if (hasReturnStmt(ifStmt.getElseStmts())) {
                    return true;
                }
            } else if (statement instanceof LoopStmt) {
                LoopStmt loopStmt = (LoopStmt) statement;
                if (hasReturnStmt(loopStmt.getStatements())) {
                    return true;
                }
            }
        }
        return false;
    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
