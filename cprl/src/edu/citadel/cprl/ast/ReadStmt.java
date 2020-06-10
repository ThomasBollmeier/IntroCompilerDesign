package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;
import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.Type;

import java.io.IOException;


/**
 * The abstract syntax tree node for a read statement.
 */
public class ReadStmt extends Statement {

    private Variable variable;


    /**
     * Construct an input statement with the specified
     * variable for storing the input.
     */
    public ReadStmt(Variable variable) {
      this.variable = variable;
    }


    @Override
    public void checkConstraints() {
        // input is limited to integers and characters
      try {

        Type type = variable.getType();

        if (type != Type.Integer && type != Type.Char) {
          throw error(variable.getPosition(),
                  "Type of variable in read statement must be Integer or Character");
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
