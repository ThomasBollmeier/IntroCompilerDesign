package edu.citadel.cprl.ast;


import edu.citadel.compiler.ConstraintException;
import edu.citadel.compiler.ErrorHandler;
import edu.citadel.cprl.ArrayType;
import edu.citadel.cprl.Symbol;
import edu.citadel.cprl.Token;
import edu.citadel.cprl.Type;


/**
 * The abstract syntax tree node for an array type declaration.
 */
public class ArrayTypeDecl extends InitialDecl {
    private final ConstValue numElements;
    private final Type elemType;     // type of elements in the array


    /**
     * Construct an ArrayTypeDecl with its identifier and element type.
     * Note that the index type is always Integer in CPRL.
     */
    public ArrayTypeDecl(Token typeId, Type elemType, ConstValue numElements) {
        super(typeId, new ArrayType(typeId.getText(), numElements.getLiteralIntValue(), elemType));
        this.elemType = elemType;
        this.numElements = numElements;
    }


    /**
     * Returns the number of elements in the array type definition.
     */
    public ConstValue getNumElements() {
        return numElements;
    }


    /**
     * Returns the type of the elements in the array.
     */
    public Type getElementType() {
        return elemType;
    }


    @Override
    public void checkConstraints() {

        try {

            Token literal = numElements.getLiteral();

            if (literal.getSymbol() != Symbol.intLiteral) {
                throw error(literal.getPosition(),
                        "Number of array elements must be an integer");
            }

            int numElementsValue = Integer.parseInt(literal.getText());
            if (numElementsValue <= 0) {
                throw error(literal.getPosition(),
                        "Number of array elements must be positive");
            }

        } catch (ConstraintException e) {
            ErrorHandler.getInstance().reportError(e);
        }

    }
}
