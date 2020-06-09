package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.Token;

import java.util.Iterator;
import java.util.List;
import java.io.IOException;


/**
 * The abstract syntax tree node for a procedure call statement.
 */
public class ProcedureCallStmt extends Statement {
    private Token procId;
    private List<Expression> actualParams;
    private ProcedureDecl procDecl;


    /*
     * Construct a procedure call statement with its name token, the
     * list of actual parameters being passed as part of the call,
     * and a reference to the declaration of the procedure being called.
     */
    public ProcedureCallStmt(Token procId,
                             List<Expression> actualParams,
                             ProcedureDecl procDecl) {
// ...
    }


    @Override
    public void checkConstraints() {
        try {

            List<ParameterDecl> formalParams = procDecl.getFormalParams();

            // check that numbers of parameters match
            if (actualParams.size() != formalParams.size())
                throw error(procId.getPosition(), "Incorrect number of actual parameters.");

            // call checkConstraints for each actual parameter
            for (Expression expr : actualParams)
                expr.checkConstraints();

            // check that parameter types match
            Iterator<Expression> iterActual = actualParams.iterator();
            Iterator<ParameterDecl> iterFormal = formalParams.iterator();

            while (iterActual.hasNext())
            {
                Expression    expr  = iterActual.next();
                ParameterDecl param = iterFormal.next();

                if (!matchTypes(expr.getType(), param.getType()))
                {
                    String errorMsg = "Parameter type mismatch.";
                    throw error(expr.getPosition(), errorMsg);
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
