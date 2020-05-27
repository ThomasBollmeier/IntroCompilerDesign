package edu.citadel.cprl

import edu.citadel.compiler.ErrorHandler
import edu.citadel.compiler.InternalCompilerException
import edu.citadel.compiler.ParserException
import edu.citadel.compiler.Position
import java.io.IOException
import java.util.*

/**
 * This class uses recursive descent to perform syntax analysis of
 * the CPRL source language.
 */
class Parser(private val scanner: Scanner) {

    companion object {
        /**
         * Symbols that can follow an initial declaration.
         */
        private val initialDeclFollowers = arrayOf<Symbol>()

        /**
         * Symbols that can follow a subprogram declaration.
         */
        private val subprogDeclFollowers = arrayOf<Symbol>()

        /**
         * Symbols that can follow a statement.
         */
        private val stmtFollowers = arrayOf(
                Symbol.identifier, Symbol.ifRW, Symbol.whileRW, Symbol.loopRW,
                Symbol.exitRW, Symbol.readRW, Symbol.writeRW, Symbol.writelnRW,
                Symbol.returnRW, Symbol.endRW
        )

        /**
         * Symbols that can follow a factor.
         */
        private val factorFollowers = arrayOf(
                Symbol.semicolon, Symbol.loopRW, Symbol.thenRW, Symbol.rightParen,
                Symbol.andRW, Symbol.orRW, Symbol.equals, Symbol.notEqual,
                Symbol.lessThan, Symbol.lessOrEqual, Symbol.greaterThan, Symbol.greaterOrEqual,
                Symbol.plus, Symbol.minus, Symbol.times, Symbol.divide,
                Symbol.modRW, Symbol.rightBracket, Symbol.comma
        )
    }

    private val idTable: IdTable = IdTable()

    /**
     * Parse the following grammar rule:<br></br>
     * `program = declarativePart statementPart "." .`
     */
    @Throws(IOException::class)
    fun parseProgram() {
        try {
            parseDeclarativePart()
            parseStatementPart()
            match(Symbol.dot)
            match(Symbol.EOF)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = arrayOf(Symbol.EOF))
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `declarativePart = initialDecls subprogramDecls .`
     */
    @Throws(IOException::class)
    fun parseDeclarativePart() {
        parseInitialDecls()
        parseSubprogramDecls()
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `initialDecls = ( initialDecl )* .`
     */
    @Throws(IOException::class)
    fun parseInitialDecls() {
        while (scanner.symbol!!.isInitialDeclStarter) parseInitialDecl()
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `initialDecl = constDecl | arrayTypeDecl | varDecl .`
     */
    @Throws(IOException::class)
    fun parseInitialDecl() {
        try {
            when (scanner.symbol) {
                Symbol.constRW -> parseConstDecl()
                Symbol.typeRW -> parseArrayTypeDecl()
                Symbol.varRW -> parseVarDecl()
                else -> throw error("Unexpected symbol '${scanner.symbol}'")
            }
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `constDecl = "const" constId ":=" literal ";" .`
     */
    @Throws(IOException::class)
    fun parseConstDecl() {
        try {
            match(Symbol.constRW)
            val constId = scanner.token
            match(Symbol.identifier)
            idTable.add(constId, IdType.constantId)
            match(Symbol.assign)
            parseLiteral()
            match(Symbol.semicolon)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
        }

    }

    /**
     * Parse the following grammar rules:<br></br>
     * `literal = intLiteral | charLiteral | stringLiteral | booleanLiteral .
     * booleanLiteral = "true" | "false" .`
     */
    @Throws(IOException::class)
    fun parseLiteral() {
        try {
            if (scanner.symbol!!.isLiteral)
                matchCurrentSymbol()
            else
                throw error("Invalid literal expression")
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = factorFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `varDecl = "var" identifiers ":" typeName ";" .`
     */
    @Throws(IOException::class)
    fun parseVarDecl() {
        try {
            match(Symbol.varRW)
            val identifiers = parseIdentifiers()
            match(Symbol.colon)
            parseTypeName()
            match(Symbol.semicolon)
            for (identifier in identifiers) idTable.add(identifier, IdType.variableId)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `identifiers = identifier ( "," identifier )* .`
     *
     * @return the list of identifier tokens.  Returns an empty list if parsing fails.
     */
    @Throws(IOException::class)
    fun parseIdentifiers(): List<Token?> {
        return try {
            val identifiers: MutableList<Token?> = ArrayList(10)
            var idToken = scanner.token
            match(Symbol.identifier)
            identifiers.add(idToken)
            while (scanner.symbol == Symbol.comma) {
                matchCurrentSymbol()
                idToken = scanner.token
                match(Symbol.identifier)
                identifiers.add(idToken)
            }
            identifiers
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = arrayOf(Symbol.colon))
            emptyList()
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `arrayTypeDecl = "type" typeId "=" "array" "[" intConstValue "]"
     * "of" typeName ";" .`
     */
    @Throws(IOException::class)
    fun parseArrayTypeDecl() {
        try {
            match(Symbol.typeRW)
            val typeId = scanner.token
            match(Symbol.identifier)
            idTable.add(typeId, IdType.arrayTypeId)
            match(Symbol.equals)
            match(Symbol.arrayRW)
            match(Symbol.leftBracket)
            parseConstValue()
            match(Symbol.rightBracket)
            match(Symbol.ofRW)
            parseTypeName()
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
        }

    }

    /**
     * Parse the following grammar rule:<br></br>
     * `typeName = "Integer" | "Boolean" | "Char" | typeId .`
     */
    @Throws(IOException::class)
    fun parseTypeName() {
        try {
            when (scanner.symbol) {
                Symbol.IntegerRW, Symbol.BooleanRW, Symbol.CharRW -> {
                    matchCurrentSymbol()
                }
                Symbol.identifier -> {
                    val typeId = scanner.token
                    matchCurrentSymbol()
                    val idType = idTable[typeId]
                    if (idType != null) {
                        if (idType != IdType.arrayTypeId) throw error(typeId!!.position, "Identifier \""
                                + typeId + "\" is not a valid type name.")
                    } else throw error(typeId!!.position, "Identifier \""
                            + typeId + "\" has not been declared.")
                }
                else -> {
                    throw error("Invalid type name.")
                }
            }
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = arrayOf(Symbol.semicolon, Symbol.comma,
                    Symbol.rightParen, Symbol.isRW))
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `subprogramDecls = ( subprogramDecl )* .`
     */
    @Throws(IOException::class)
    fun parseSubprogramDecls() {
        while (scanner.symbol?.isSubprogramDeclStarter == true) {
            parseSubprogramDecl()
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `subprogramDecl = procedureDecl | functionDecl .`
     */
    @Throws(IOException::class)
    fun parseSubprogramDecl() {
        try {
            when (scanner.symbol) {
                Symbol.procedureRW -> parseProcedureDecl()
                Symbol.functionRW -> parseFunctionDecl()
                else -> throw error("Unexpected symbol '${scanner.symbol}'")
            }
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = subprogDeclFollowers)
        }

// ...   throw an internal error if the symbol is not one of procedureRW or functionRW
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `procedureDecl = "procedure" procId ( formalParameters )?
     * "is" initialDecls statementPart procId ";" .`
     */
    @Throws(IOException::class)
    fun parseProcedureDecl() {
        try {
            match(Symbol.procedureRW)
            val procId = scanner.token
            match(Symbol.identifier)
            idTable.add(procId, IdType.procedureId)
            idTable.openScope()
            if (scanner.symbol == Symbol.leftParen) parseFormalParameters()
            match(Symbol.isRW)
            parseInitialDecls()
            parseStatementPart()
            idTable.closeScope()
            val procId2 = scanner.token
            match(Symbol.identifier)
            if (procId!!.text != procId2!!.text) throw error(procId2.position, "Procedure name mismatch.")
            match(Symbol.semicolon)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = subprogDeclFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `functionDecl = "function" funcId ( formalParameters )? "return" typeName
     * "is" initialDecls statementPart funcId ";" .`
     */
    @Throws(IOException::class)
    fun parseFunctionDecl() {
        try {
            match(Symbol.functionRW)
            val funcId = scanner.token
            match(Symbol.identifier)
            idTable.add(funcId, IdType.functionId)
            idTable.openScope()
            if (scanner.symbol == Symbol.leftParen) parseFormalParameters()
            match(Symbol.returnRW)
            parseTypeName()
            match(Symbol.isRW)
            parseInitialDecls()
            parseStatementPart()
            idTable.closeScope()
            val funcId2 = scanner.token
            match(Symbol.identifier)
            if (funcId!!.text != funcId2!!.text) throw error(funcId2.position, "Procedure name mismatch.")
            match(Symbol.semicolon)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = subprogDeclFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `formalParameters = "(" parameterDecl ( "," parameterDecl )* ")" .`
     */
    @Throws(IOException::class)
    fun parseFormalParameters() {
        match(Symbol.leftParen)
        parseParameterDecl()
        while (scanner.symbol == Symbol.comma) {
            matchCurrentSymbol()
            parseParameterDecl()
        }
        match(Symbol.rightParen)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `parameterDecl = ( "var" )? paramId ":" typeName .`
     */
    @Throws(IOException::class)
    fun parseParameterDecl() {
        if (scanner.symbol == Symbol.varRW) {
            matchCurrentSymbol()
        }
        val paramId = scanner.token
        match(Symbol.identifier)
        idTable.add(paramId, IdType.variableId)
        match(Symbol.colon)
        parseTypeName()
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statementPart = "begin" statements "end" .`
     */
    @Throws(IOException::class)
    fun parseStatementPart() {
        try {
            match(Symbol.beginRW)
            parseStatements()
            match(Symbol.endRW)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = arrayOf(Symbol.dot, Symbol.identifier))
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statements = ( statement )* .`
     */
    @Throws(IOException::class)
    fun parseStatements() {
        while (scanner.symbol?.isStmtStarter == true) {
            parseStatement()
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statement = assignmentStmt | ifStmt | loopStmt | exitStmt | readStmt
     * | writeStmt | writelnStmt | procedureCallStmt | returnStmt .`
     */
    @Throws(IOException::class)
    fun parseStatement() {
        // assumes that scanner.getSymbol() can start a statement
        assert(scanner.symbol!!.isStmtStarter) { "Invalid statement." }

        try {

            when (scanner.symbol) {
                Symbol.identifier -> {
                    when (idTable.get(scanner.token)) {
                        IdType.functionId, IdType.procedureId -> parseProcedureCallStmt()
                        IdType.variableId -> parseAssignmentStmt()
                        else -> throw error("Identifier \"${scanner.token?.text}\" cannot start a statement")
                    }
                }
                Symbol.ifRW -> parseIfStmt()
                Symbol.whileRW, Symbol.loopRW -> parseLoopStmt()
                Symbol.exitRW -> parseExitStmt()
                Symbol.readRW -> parseReadStmt()
                Symbol.writeRW -> parseWriteStmt()
                Symbol.writelnRW -> parseWritelnStmt()
                Symbol.returnRW -> parseReturnStmt()
                else -> throw error("Unexpected symbol '${scanner.symbol}'")
            }

        } catch (error: ParserException) {
            ErrorHandler.getInstance().reportError(error)
            scanner.advanceTo(Symbol.semicolon)
            recover(stmtFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `assignmentStmt = variable ":=" expression ";" .`
     */
    @Throws(IOException::class)
    fun parseAssignmentStmt() {
        parseVariable()
        match(Symbol.assign)
        parseExpression()
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `ifStmt = "if" booleanExpr "then" statements
     * ( "elsif" booleanExpr "then" statements )*
     * ( "else" statements )? "end" "if" ";" .
    ` */
    @Throws(IOException::class)
    fun parseIfStmt() {
        match(Symbol.ifRW)
        parseExpression()
        match(Symbol.thenRW)
        parseStatements()
        while (scanner.symbol == Symbol.elsifRW) {
            parseExpression()
            match(Symbol.thenRW)
            parseStatements()
        }
        if (scanner.symbol == Symbol.elseRW) {
            parseStatements()
        }
        match(Symbol.endRW)
        match(Symbol.ifRW)
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `loopStmt = ( "while" booleanExpr )? "loop"
     * statements "end" "loop" ";" .`
     */
    @Throws(IOException::class)
    fun parseLoopStmt() {
        if (scanner.symbol == Symbol.whileRW) {
            matchCurrentSymbol()
            parseExpression()
        }
        match(Symbol.loopRW)
        parseStatements()
        match(Symbol.endRW)
        match(Symbol.loopRW)
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `exitStmt = "exit" ( "when" booleanExpr )? ";" .`
     */
    @Throws(IOException::class)
    fun parseExitStmt() {
        match(Symbol.exitRW)
        if (scanner.symbol == Symbol.whenRW) {
            matchCurrentSymbol()
            parseExpression()
        }
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `readStmt = "read" variable ";" .`
     */
    @Throws(IOException::class)
    fun parseReadStmt() {
        match(Symbol.readRW)
        parseVariable()
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `writeStmt = "write" expressions ";" .`
     */
    @Throws(IOException::class)
    fun parseWriteStmt() {
        match(Symbol.writeRW)
        parseExpressions()
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `expressions = expression ( "," expression )* .`
     */
    @Throws(IOException::class)
    fun parseExpressions() {
        try {
            parseExpression()
            while (scanner.symbol == Symbol.comma) {
                matchCurrentSymbol()
                parseExpression()
            }
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            val exprsFollowers = emptyArray<Symbol>()
            recover(exprsFollowers)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `writelnStmt = "writeln" ( expressions )? ";" .`
     */
    @Throws(IOException::class)
    fun parseWritelnStmt() {
        match(Symbol.writelnRW)
        if (scanner.symbol!!.isExprStarter) parseExpressions()
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `procedureCallStmt = procId ( actualParameters )? ";" .`
     */
    @Throws(IOException::class)
    fun parseProcedureCallStmt() {
        val idType = idTable.get(scanner.token)
        if (idType != IdType.procedureId) {
            throw error("Procedure expected")
        }
        match(Symbol.identifier)
        if (scanner.symbol == Symbol.leftParen) {
            parseActualParameters()
        }
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `actualParameters = "(" expressions ")" .`
     */
    @Throws(IOException::class)
    fun parseActualParameters() {
        match(Symbol.leftParen)
        parseExpressions()
        match(Symbol.rightParen)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `returnStmt = "return" ( expression )? ";" .`
     */
    @Throws(IOException::class)
    fun parseReturnStmt() {
        match(Symbol.returnRW)
        if (scanner.symbol != Symbol.semicolon) {
            parseExpression()
        }
        match(Symbol.semicolon)
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `variable = ( varId | paramId ) ( "[" expression "]" )* .`
     * <br></br>
     * This helper method provides common logic for methods parseVariable() and
     * parseNamedValue().  The method does not handle any ParserExceptions but
     * throws them back to the calling method where they can be handled appropriately.
     *
     * @throws ParserException if parsing fails.
     * @see .parseVariable
     * @see .parseNamedValue
     */
    @Throws(IOException::class, ParserException::class)
    fun parseVariableExpr() {
        val idToken = scanner.token
        match(Symbol.identifier)
        val idType = idTable[idToken]
        if (idType == null) {
            val errorMsg = "Identifier \"$idToken\" has not been declared."
            throw error(idToken!!.position, errorMsg)
        } else if (idType != IdType.variableId) {
            val errorMsg = "Identifier \"$idToken\" is not a variable."
            throw error(idToken!!.position, errorMsg)
        }
        while (scanner.symbol == Symbol.leftBracket) {
            matchCurrentSymbol()
            parseExpression()
            match(Symbol.rightBracket)
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `variable = ( varId | paramId ) ( "[" expression "]" )* .`
     */
    @Throws(IOException::class)
    fun parseVariable() {
        parseVariableExpr()
    }

    /**
     * Parse the following grammar rules:<br></br>
     * `expression = relation ( logicalOp relation )* .<br></br>
     * logicalOp = "and" | "or" .`
     */
    @Throws(IOException::class)
    fun parseExpression() {
        parseRelation()
        while (scanner.symbol!!.isLogicalOperator) {
            matchCurrentSymbol()
            parseRelation()
        }
    }

    /**
     * Parse the following grammar rules:<br></br>
     * `relation = simpleExpr ( relationalOp simpleExpr )? .<br></br>
     * relationalOp = "=" | "!=" | "<" | "<=" | ">" | ">=" .`
     */
    @Throws(IOException::class)
    fun parseRelation() {
        parseSimpleExpr()
        while (scanner.symbol?.isRelationalOperator == true) {
            matchCurrentSymbol()
            parseSimpleExpr()
        }
    }

    /**
     * Parse the following grammar rules:<br></br>
     * `simpleExpr = ( addingOp )? term ( addingOp term )* .<br></br>
     * addingOp = "+" | "-" .`
     */
    @Throws(IOException::class)
    fun parseSimpleExpr() {
        if (scanner.symbol?.isAddingOperator == true) {
            matchCurrentSymbol()
        }
        parseTerm()
        while (scanner.symbol?.isAddingOperator == true) {
            matchCurrentSymbol()
            parseTerm()
        }
    }

    /**
     * Parse the following grammar rules:<br></br>
     * `term = factor ( multiplyingOp factor )* .<br></br>
     * multiplyingOp = "*" | "/" | "mod" .`
     */
    @Throws(IOException::class)
    fun parseTerm() {
        parseFactor()
        while (scanner.symbol?.isMultiplyingOperator == true) {
            matchCurrentSymbol()
            parseFactor()
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `factor = "not" factor | constValue | namedValue | functionCall
     * | "(" expression ")" .`
     */
    @Throws(IOException::class)
    fun parseFactor() {
        if (scanner.symbol == Symbol.notRW) {
            matchCurrentSymbol()
            parseFactor()
        } else if (scanner.symbol!!.isLiteral) {
            // Handle constant literals separately from constant identifiers.
            parseConstValue()
        } else if (scanner.symbol == Symbol.identifier) {
            // Handle identifiers based on whether they are
            // declared as variables, constants, or functions.
            val idToken = scanner.token
            val idType = idTable[idToken]
            if (idType != null) {
                when (idType) {
                    IdType.constantId -> parseConstValue()
                    IdType.variableId -> parseNamedValue()
                    IdType.functionId -> parseFunctionCall()
                    else -> throw error("Identifier \"" + scanner.token
                            + "\" is not valid as an expression.")
                }
            } else throw error("Identifier \"" + scanner.token
                    + "\" has not been declared.")
        } else if (scanner.symbol == Symbol.leftParen) {
            matchCurrentSymbol()
            parseExpression()
            match(Symbol.rightParen)
        } else throw error("Invalid expression")
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `constValue = literal | constId .`
     */
    @Throws(IOException::class)
    fun parseConstValue() {
        if (scanner.symbol?.isLiteral == true) {
            parseLiteral()
        } else {
            val idType = idTable.get(scanner.token)
            if (idType == IdType.constantId) {
                matchCurrentSymbol()
            } else {
                throw error("Constant identifier expected")
            }
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `namedValue = variable .`
     */
    @Throws(IOException::class)
    fun parseNamedValue() {
        parseVariableExpr()
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `functionCall = functId ( actualParameters )? .`
     */
    @Throws(IOException::class)
    fun parseFunctionCall() {
        val idType = idTable.get(scanner.token)
        if (idType != IdType.functionId) {
            throw error("Function expected")
        }
        match(Symbol.identifier)
        if (scanner.symbol == Symbol.leftParen) {
            parseActualParameters()
        }
    }
    // Utility parsing methods
    /**
     * Check that the current scanner symbol is the expected symbol.  If it is,
     * then advance the scanner.  Otherwise, throw a ParserException object.
     */
    @Throws(IOException::class, ParserException::class)
    private fun match(expectedSymbol: Symbol) {
        if (scanner.symbol == expectedSymbol) scanner.advance() else {
            val errorMsg = ("Expecting \"" + expectedSymbol + "\" but found \""
                    + scanner.symbol + "\" instead.")
            throw error(errorMsg)
        }
    }

    /**
     * Advance the scanner.  This method represents an unconditional match
     * with the current scanner symbol.
     */
    @Throws(IOException::class)
    private fun matchCurrentSymbol() {
        scanner.advance()
    }

    /**
     * Create a parser exception with the specified message and the
     * current scanner position.
     */
    private fun error(errMsg: String): ParserException {
        val errPos = scanner.token!!.position
        return ParserException(errPos, errMsg)
    }

    /**
     * Create a parser exception with the specified error position and message.
     */
    private fun error(errPos: Position, errMsg: String): ParserException {
        return ParserException(errPos, errMsg)
    }

    /**
     * Create an internal compiler exception with the specified message
     * and the current scanner position.
     */
    private fun internalError(message: String): InternalCompilerException {
        val errorPosition = scanner.token!!.position
        return InternalCompilerException(errorPosition, message)
    }

    /**
     * Advance the scanner until the current symbol is one of the
     * symbols in the specified array of follows.
     */
    @Throws(IOException::class)
    private fun recover(followers: Array<Symbol>) {
        scanner.advanceTo(followers)
    }
}