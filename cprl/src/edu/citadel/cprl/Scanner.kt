package edu.citadel.cprl

import edu.citadel.compiler.ErrorHandler
import edu.citadel.compiler.Position
import edu.citadel.compiler.ScannerException
import edu.citadel.compiler.Source
import java.io.IOException

class Scanner(private val source: Source) {

    private val scanBuffer: StringBuilder = StringBuilder(100)
    private val reservedWords = Symbol.values()
            .filter { it.isReservedWord }
            .associateBy { it.toString() }
    private val singleCharSymbols = Symbol.values()
            .filter { it.toString().length == 1 }
            .associateBy { it.toString() }

    private val tokenBuffer = mutableListOf<Token>()

    private var position : Position? = null

    /**
     * Returns a copy of the current token in the source file.
     */
    val token
        get() = tokenBuffer.getOrNull(0)

    val symbol
        get() = peekSymbol(0)

    fun peekToken(idx: Int) : Token? {
        while (tokenBuffer.size < idx + 1) {
            var token = nextToken()
            tokenBuffer.add(token)
            if (token.symbol == Symbol.EOF) {
                break
            }
        }

        return tokenBuffer.getOrNull(idx)
    }

    fun peekSymbol(idx: Int) = peekToken(idx)?.symbol

    /**
     * Advance to the next token in the source file.
     */
    @Throws(IOException::class)
    fun advance() {
        if (tokenBuffer.isNotEmpty()) {
            tokenBuffer.removeAt(0)
        }
        tokenBuffer.add(nextToken())
    }

    private fun nextToken() : Token {

        var symbol: Symbol? = null
        position = null
        var text: String? = null

        advanceloop@ while (true) {

            try {
                skipWhiteSpace()

                // currently at starting character of next token
                position = source.charPosition
                text = null
                if (source.char == Source.EOF) {
                    // set symbol but don't advance
                    symbol = Symbol.EOF
                } else if (Character.isLetter(source.char.toChar())) {
                    val idString = scanIdentifier()
                    symbol = getIdentifierSymbol(idString)
                    if (symbol == Symbol.identifier) text = idString
                } else if (Character.isDigit(source.char.toChar())) {
                    symbol = Symbol.intLiteral
                    text = scanIntegerLiteral()
                } else {
                    when (source.char.toChar()) {
                        '\'' -> {
                            symbol = Symbol.charLiteral
                            text = scanCharLiteral()
                        }
                        '\"' -> {
                            symbol = Symbol.stringLiteral
                            text = scanStringLiteral()
                        }
                        '+', '-', '*', '=', '(', ')', '[', ']', ',', ';', '.' -> {
                            symbol = singleCharSymbols[source.char.toChar().toString()]
                            source.advance()
                        }
                        '/' -> {
                            source.advance()
                            if (source.char.toChar() != '/') {
                                symbol = Symbol.divide
                            } else {
                                skipComment()
                                continue@advanceloop
                            }
                        }
                        '!' -> {
                            source.advance()
                            if (source.char.toChar() == '=') {
                                symbol = Symbol.notEqual
                                source.advance()
                            } else {
                                val errorMsg = ("Invalid character '!'")
                                throw error(errorMsg)
                            }
                        }
                        '>' -> {
                            source.advance()
                            if (source.char.toChar() == '=') {
                                symbol = Symbol.greaterOrEqual
                                source.advance()
                            } else symbol = Symbol.greaterThan
                        }
                        '<' -> {
                            source.advance()
                            if (source.char.toChar() == '=') {
                                symbol = Symbol.lessOrEqual
                                source.advance()
                            } else symbol = Symbol.lessThan
                        }
                        ':' -> {
                            source.advance()
                            if (source.char.toChar() == '=') {
                                symbol = Symbol.assign
                                source.advance()
                            } else symbol = Symbol.colon
                        }
                        else -> {
                            val errorMsg = ("Invalid character \'"
                                    + source.char.toChar() + "\'")
                            source.advance()
                            throw error(errorMsg)
                        }
                    }
                }
            } catch (e: ScannerException) {
                ErrorHandler.getInstance().reportError(e)

                // set token to either EOF or unknown
                if (source.char == Source.EOF) {
                    if (symbol != Symbol.EOF) symbol = Symbol.EOF
                } else symbol = Symbol.unknown
            }

            break@advanceloop
        }

        return Token(symbol, position, text)
    }

    /**
     * Returns the symbol associated with an identifier
     * (Symbol.arrayRW, Symbol.ifRW, Symbol.identifier, etc.)
     */
    private fun getIdentifierSymbol(idString: String): Symbol =
            reservedWords[idString] ?: Symbol.identifier

    /**
     * Skip over a comment.
     */
    @Throws(ScannerException::class, IOException::class)
    private fun skipComment() {
        skipToEndOfLine()
        source.advance()
    }

    /**
     * Advance until the symbol in the source file matches the symbol
     * specified in the parameter or until end of file is encountered.
     */
    @Throws(IOException::class)
    fun advanceTo(destSymbol: Symbol) {
        while (true) {
            if (symbol == destSymbol || source.char == Source.EOF) return else advance()
        }
    }

    /**
     * Advance until the symbol in the source file matches one of the
     * symbols in the given array or until end of file is encountered.
     */
    @Throws(IOException::class)
    fun advanceTo(symbols: Array<Symbol>) {
        while (true) {
            if (search(
                            symbols,
                            symbol
                    ) >= 0 || source.char == Source.EOF
            ) return else advance()
        }
    }

    /**
     * Performs a linear search of the array for the given value.
     *
     * @return the index of the value in the array if found, otherwise -1.
     */
    private fun search(symbols: Array<Symbol>, value: Symbol?): Int {
        for (i in symbols.indices) {
            if (symbols[i] == value) return i
        }
        return -1
    }

    /**
     * Clear the scan buffer (makes it empty).
     */
    private fun clearScanBuffer() {
        scanBuffer.delete(0, scanBuffer.length)
    }

    /**
     * Scans characters in the source file for a valid identifier using the
     * lexical rule: identifier = letter ( letter | digit)* .
     *
     * @return the string of letters and digits for the identifier.
     */
    @Throws(IOException::class)
    private fun scanIdentifier(): String {

        var ch = source.char.toChar()
        assert(Character.isLetter(ch)) {
            "scanIdentifier(): Letter expected at position $position"
        }

        clearScanBuffer()

        do {
            scanBuffer.append(ch)
            source.advance()
            ch = source.char.toChar()
        } while (Character.isLetterOrDigit(ch) || ch == '_')

        return scanBuffer.toString()
    }

    /**
     * Scans characters in the source file for a valid integer literal.
     * Assumes that source.getChar() is the first character of the Integer literal.
     *
     * @return the string of digits for the integer literal.
     */
    @Throws(ScannerException::class, IOException::class)
    private fun scanIntegerLiteral(): String {
        // assumes that source.getChar() is the first digit of the integer literal
        assert(Character.isDigit(source.char.toChar())) {
            ("scanIntegerLiteral(): check integer literal start for digit at position "
                    + position)
        }
        clearScanBuffer()
        do {
            scanBuffer.append(source.char.toChar())
            source.advance()
        } while (Character.isDigit(source.char.toChar()))
        return scanBuffer.toString()
    }

    /**
     * Scan characters in the source file for a String literal.  Escaped
     * characters are not converted; e.g., '\t' is not converted to the tab
     * character since the assembler performs the conversion.  Assumes that
     * source.getChar() is the opening double quote (") of the String literal.
     *
     * @return the string of characters for the string literal, including
     * opening and closing quotes
     */
    @Throws(ScannerException::class, IOException::class)
    private fun scanStringLiteral(): String {

        var ch = source.char.toChar()
        assert(ch == '"') {
            "scanStringLiteral(): '\"' expected at position $position"
        }

        clearScanBuffer()

        var done = false
        scanBuffer.append(ch)
        source.advance()

        do {
            checkGraphicChar(source.char)
            ch = source.char.toChar()
            when (ch) {
                '\\' -> scanBuffer.append(scanEscapedChar())
                else -> {
                    done = ch == '"'
                    scanBuffer.append(ch)
                    source.advance()
                }
            }
        } while (!done)

        return scanBuffer.toString()
    }

    /**
     * Scan characters in the source file for a Char literal.  Escaped
     * characters are not converted; e.g., '\t' is not converted to the tab
     * character since the assembler performs that conversion.  Assumes that
     * source.getChar() is the opening single quote (') of the Char literal.
     *
     * @return the string of characters for the char literal, including
     * opening and closing single quotes.
     */
    @Throws(ScannerException::class, IOException::class)
    private fun scanCharLiteral(): String {
        // assumes that source.getChar() is the opening single quote for the char literal
        assert(
                source.char.toChar() == '\''
        ) { "scanCharLiteral(): check for opening quote (\') at position $position" }
        val errorMsg = "Invalid Char literal."
        clearScanBuffer()

        // append the opening single quote
        var c = source.char.toChar()
        scanBuffer.append(c)
        source.advance()
        checkGraphicChar(source.char)
        c = source.char.toChar()
        if (c == '\\') // escaped character
        {
            scanBuffer.append(scanEscapedChar())
        } else if (c == '\'') // either '' (empty) or '''; both are invalid
        {
            source.advance()
            c = source.char.toChar()
            if (c == '\'') // three single quotes in a row
                source.advance()
            throw error(errorMsg)
        } else {
            scanBuffer.append(c)
            source.advance()
        }
        c = source.char.toChar() // should be the closing single quote
        checkGraphicChar(c.toInt())
        if (c == '\'') // should be the closing single quote
        {
            scanBuffer.append(c) // append the closing quote
            source.advance()
        } else throw error(errorMsg)
        return scanBuffer.toString()
    }

    /**
     * Scans characters in the source file for an escaped character; i.e.,
     * a character preceded by a backslash.  This method checks escape
     * characters \b, \t, \n, \f, \r, \", \', and \\.  If the character
     * following a backslash is anything other than one of these characters,
     * then an exception is thrown.  Note that the escaped character sequence
     * is returned unmodified; i.e., \t returns "\t", not the tab character.
     * Assumes that source.getChar() is the escape character (\).
     *
     * @return the escaped character sequence unmodified.
     */
    @Throws(ScannerException::class, IOException::class)
    private fun scanEscapedChar(): String {
        // assumes that source.getChar() is the backslash for the escaped char
        assert(source.char.toChar() == '\\') {
            ("scanEscapedChar(): check for escape character ('\\') at position "
                    + position)
        }

        // Need to save current position for error reporting.
        val backslashPosition = source.charPosition
        source.advance()
        checkGraphicChar(source.char)
        val c = source.char.toChar()
        source.advance() // leave source at second character following the backslash
        return when (c) {
            'b' -> "\\b" // backspace
            't' -> "\\t" // tab
            'n' -> "\\n" // linefeed (a.k.a. newline)
            'f' -> "\\f" // form feed
            'r' -> "\\r" // carriage return
            '\"' -> "\\\"" // double quote
            '\'' -> "\\\'" // single quote
            '\\' -> "\\\\" // backslash
            else -> {
                val errMessage = "Illegal escape character."
                val ex = ScannerException(backslashPosition, errMessage)
                ErrorHandler.getInstance().reportError(ex)
                "\\c"
            }
        }
    }

    /**
     * Fast skip over white space.
     */
    @Throws(IOException::class)
    private fun skipWhiteSpace() {
        while (Character.isWhitespace(source.char.toChar())) {
            source.advance()
        }
    }

    /**
     * Advances over source characters to the end of the current line.
     */
    @Throws(ScannerException::class, IOException::class)
    private fun skipToEndOfLine() {
        while (source.char.toChar() != '\n') {
            source.advance()
            checkEOF()
        }
    }

    /**
     * Checks that the integer represents a graphic character in the Unicode
     * Basic Multilingual Plane (BMP).
     *
     * @throws ScannerException if the integer does not represent a BMP graphic
     * character.
     */
    @Throws(ScannerException::class)
    private fun checkGraphicChar(n: Int) {
        if (n == Source.EOF) throw error("End of file reached before closing quote for Char or String literal.") else if (n > 0xffff) throw error(
                "Character not in Unicode Basic Multilingual Pane (BMP)"
        ) else {
            val c = n.toChar()
            if (c == '\r' || c == '\n') throw error("Char and String literals can not extend past end of line.") else if (Character.isISOControl(
                            c
                    )
            ) throw ScannerException(
                    source.charPosition,
                    "Control characters not allowed in Char or String literal."
            )
        }
    }

    /**
     * Returns a scanner exception with the specified error message.
     */
    private fun error(errorMsg: String): ScannerException {
        return ScannerException(position, errorMsg)
    }

    /**
     * Used to check for EOF in the middle of scanning tokens that
     * require closing characters such as strings and comments.
     *
     * @throws ScannerException if source is at end of file.
     */
    @Throws(ScannerException::class)
    private fun checkEOF() {
        if (source.char == Source.EOF) throw ScannerException(
                position,
                "Unexpected end of file"
        )
    }

    /**
     * Initialize scanner with its associated source and advance
     * to the first token.
     */
    init {
        advance() // advance to the first token
    }
}
