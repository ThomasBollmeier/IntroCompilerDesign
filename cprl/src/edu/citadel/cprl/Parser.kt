package edu.citadel.cprl

import edu.citadel.compiler.ErrorHandler
import edu.citadel.compiler.InternalCompilerException
import edu.citadel.compiler.ParserException
import edu.citadel.compiler.Position
import edu.citadel.cprl.ast.*
import java.io.IOException
import java.lang.reflect.Parameter
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
        private val initialDeclFollowers = arrayOf(
                Symbol.constRW, Symbol.varRW, Symbol.typeRW,
                Symbol.procedureRW, Symbol.functionRW, Symbol.beginRW
        )

        /**
         * Symbols that can follow a subprogram declaration.
         */
        private val subprogDeclFollowers = arrayOf(
                Symbol.procedureRW, Symbol.functionRW, Symbol.beginRW
        )

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
    fun parseInitialDecls(): List<InitialDecl?> {

        val result = mutableListOf<InitialDecl?>()
        while (scanner.symbol!!.isInitialDeclStarter) {
            result.add(parseInitialDecl())
        }

        return result
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `initialDecl = constDecl | arrayTypeDecl | varDecl .`
     */
    @Throws(IOException::class)
    fun parseInitialDecl(): InitialDecl? {
        return try {
            when (scanner.symbol) {
                Symbol.constRW -> parseConstDecl()
                Symbol.typeRW -> parseArrayTypeDecl()
                Symbol.varRW -> parseVarDecl()
                else -> throw error("Unexpected symbol '${scanner.symbol}'")
            }
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `constDecl = "const" constId ":=" literal ";" .`
     */
    @Throws(IOException::class)
    fun parseConstDecl(): ConstDecl? {
        return try {
            match(Symbol.constRW)
            val identifier = scanner.token
            match(Symbol.identifier)
            match(Symbol.assign)
            val literal = parseLiteral()
            match(Symbol.semicolon)
            ConstDecl(identifier, Type.getTypeOf(literal?.symbol), literal)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
            null
        }

    }

    /**
     * Parse the following grammar rules:<br></br>
     * `literal = intLiteral | charLiteral | stringLiteral | booleanLiteral .
     * booleanLiteral = "true" | "false" .`
     */
    @Throws(IOException::class)
    fun parseLiteral(): Token? {
        return try {
            if (scanner.symbol!!.isLiteral) {
                val result = scanner.token
                matchCurrentSymbol()
                result
            }
            else
                throw error("Invalid literal expression")
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = factorFollowers)
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `varDecl = "var" identifiers ":" typeName ";" .`
     */
    @Throws(IOException::class)
    fun parseVarDecl(): VarDecl? {
        try {
            match(Symbol.varRW)
            val identifiers = parseIdentifiers()
            match(Symbol.colon)
            val type = parseTypeName()
            match(Symbol.semicolon)

            val varDecl = VarDecl(identifiers, type, idTable.currentLevel)
            for (singleVarDecl in varDecl.singleVarDecls) {
                idTable.add(singleVarDecl)
            }
            return varDecl

        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
            return null
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
    fun parseArrayTypeDecl(): ArrayTypeDecl? {
        return try {
            match(Symbol.typeRW)
            val typeId = scanner.token
            match(Symbol.identifier)
            match(Symbol.equals)
            match(Symbol.arrayRW)
            match(Symbol.leftBracket)
            val numElements = parseConstValue()
            match(Symbol.rightBracket)
            match(Symbol.ofRW)
            val elementType = parseTypeName()
            val result = ArrayTypeDecl(typeId, elementType, numElements)
            idTable.add(result)
            result
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = initialDeclFollowers)
            null
        }

    }

    /**
     * Parse the following grammar rule:<br></br>
     * `typeName = "Integer" | "Boolean" | "Char" | typeId .`
     */
    @Throws(IOException::class)
    fun parseTypeName(): Type {
        try {
            when (scanner.symbol) {
                Symbol.IntegerRW -> {
                    matchCurrentSymbol()
                    return Type.Integer
                }
                Symbol.BooleanRW -> {
                    matchCurrentSymbol()
                    return Type.Boolean
                }
                Symbol.CharRW -> {
                    matchCurrentSymbol()
                    return Type.Char
                }
                Symbol.identifier -> {
                    val typeId = scanner.token
                    matchCurrentSymbol()
                    val idType = idTable[typeId]
                    if (idType != null) {
                        return if (idType is ArrayTypeDecl) {
                            idType.type
                        } else {
                            throw error(typeId!!.position, "Identifier \""
                                    + typeId + "\" is not a valid type name.")
                        }
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
            return Type.UNKNOWN
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
    fun parseProcedureDecl(): ProcedureDecl? {
        return try {
            match(Symbol.procedureRW)
            val procId = scanner.token
            match(Symbol.identifier)
            val result = ProcedureDecl(procId)
            idTable.openScope()
            if (scanner.symbol == Symbol.leftParen) {
                result.formalParams = parseFormalParameters()
            }
            match(Symbol.isRW)
            result.setInitialDecls(parseInitialDecls())
            result.statementPart = parseStatementPart()
            idTable.closeScope()
            val procId2 = scanner.token
            match(Symbol.identifier)
            if (procId!!.text != procId2!!.text) throw error(procId2.position, "Procedure name mismatch.")
            match(Symbol.semicolon)
            idTable.add(result)
            result
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = subprogDeclFollowers)
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `functionDecl = "function" funcId ( formalParameters )? "return" typeName
     * "is" initialDecls statementPart funcId ";" .`
     */
    @Throws(IOException::class)
    fun parseFunctionDecl(): FunctionDecl? {
        return try {
            match(Symbol.functionRW)
            val funcId = scanner.token
            match(Symbol.identifier)
            val result = FunctionDecl(funcId)
            idTable.openScope()
            if (scanner.symbol == Symbol.leftParen) {
                result.formalParams = parseFormalParameters()
            }
            match(Symbol.returnRW)
            val returnType = parseTypeName()
            match(Symbol.isRW)
            result.setInitialDecls(parseInitialDecls())
            result.statementPart = parseStatementPart()
            idTable.closeScope()
            val funcId2 = scanner.token
            match(Symbol.identifier)
            if (funcId!!.text != funcId2!!.text) throw error(funcId2.position, "Procedure name mismatch.")
            match(Symbol.semicolon)
            result
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = subprogDeclFollowers)
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `formalParameters = "(" parameterDecl ( "," parameterDecl )* ")" .`
     */
    @Throws(IOException::class)
    fun parseFormalParameters(): List<ParameterDecl> {
        val result = mutableListOf<ParameterDecl>()
        match(Symbol.leftParen)
        result.add(parseParameterDecl())
        while (scanner.symbol == Symbol.comma) {
            matchCurrentSymbol()
            result.add(parseParameterDecl())
        }
        match(Symbol.rightParen)
        return result
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `parameterDecl = ( "var" )? paramId ":" typeName .`
     */
    @Throws(IOException::class)
    fun parseParameterDecl(): ParameterDecl {
        val isVarParam = if (scanner.symbol == Symbol.varRW) {
            matchCurrentSymbol()
            true
        } else {
            false
        }
        val paramId = scanner.token
        match(Symbol.identifier)
        match(Symbol.colon)
        val type = parseTypeName()
        val result = ParameterDecl(paramId, type, isVarParam)
        idTable.add(result)
        return result
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statementPart = "begin" statements "end" .`
     */
    @Throws(IOException::class)
    fun parseStatementPart(): StatementPart? {
        return try {
            match(Symbol.beginRW)
            val statements = parseStatements()
            match(Symbol.endRW)
            StatementPart(statements)
        } catch (e: ParserException) {
            ErrorHandler.getInstance().reportError(e)
            recover(followers = arrayOf(Symbol.dot, Symbol.identifier))
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statements = ( statement )* .`
     */
    @Throws(IOException::class)
    fun parseStatements(): List<Statement?> {
        val result = mutableListOf<Statement?>()
        while (scanner.symbol?.isStmtStarter == true) {
            result.add(parseStatement())
        }
        return result
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `statement = assignmentStmt | ifStmt | loopStmt | exitStmt | readStmt
     * | writeStmt | writelnStmt | procedureCallStmt | returnStmt .`
     */
    @Throws(IOException::class)
    fun parseStatement(): Statement? {
        // assumes that scanner.getSymbol() can start a statement
        assert(scanner.symbol!!.isStmtStarter) { "Invalid statement." }

        return try {

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
            null // TODO: remove
        } catch (error: ParserException) {
            ErrorHandler.getInstance().reportError(error)
            scanner.advanceTo(Symbol.semicolon)
            recover(stmtFollowers)
            null
        }
    }

    /**
     * Parse the following grammar rule:<br></br>
     * `assignmentStmt = variable ":=" expression ";" .`
     */
    @Throws(IOException::class)
    fun parseAssignmentStmt() {
        parseVariable()
        try {
            match(Symbol.assign)
        } catch (error: ParserException) {
            if (scanner.symbol == Symbol.equals) {
                ErrorHandler.getInstance().reportError(error)
                matchCurrentSymbol()
            } else {
                throw error
            }
        }
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
    fun parseConstValue(): ConstValue {
        return if (scanner.symbol?.isLiteral == true) {
            ConstValue(parseLiteral())
        } else {
            val idType = idTable.get(scanner.token)
            if (idType is ConstDecl) {
                matchCurrentSymbol()
                ConstValue(idType.literal)
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