package edu.citadel.cprl.ast;


import edu.citadel.compiler.CodeGenException;

import java.util.List;
import java.io.IOException;


/**
 * The abstract syntax tree node for the statement part of a program.
 */
public class StatementPart extends AST {
    private List<Statement> statements;


    /**
     * Construct a statement part with the specified list of statements.
     */
    public StatementPart(List<Statement> statements) {
        this.statements = statements;
    }


    public List<Statement> getStatements() {
        return statements;
    }


    @Override
    public void checkConstraints() {
        for (Statement statement: statements) {
            statement.checkConstraints();
        }
    }


    @Override
    public void emit() throws CodeGenException, IOException {
// ...
    }
}
