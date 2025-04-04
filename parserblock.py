import astnodes_ as ast
import lexeridentifiers as lex

class Parser:
    def __init__(self, src_program_str):
        self.name = "PARSEAR"
        self.lexer = lex.Lexer()
        self.index = -1  #start at -1 so that the first token is at index 0
        self.src_program = src_program_str
        self.tokens = self.lexer.GenerateTokens(self.src_program)
        #print("[Parser] Lexer generated token list ::")
        #for t in self.tokens:
        #    print(t.type, t.lexeme)
        self.crtToken = lex.Token("", lex.TokenType.error)
        self.nextToken = lex.Token("", lex.TokenType.error)
        self.ASTroot = ast.ASTProgramNode    

    #Skips any white space
    def NextTokenSkipWS(self):
        self.index += 1   #Grab the next token
        if (self.index < len(self.tokens)):
            self.crtToken = self.tokens[self.index]
        else:
            self.crtToken = lex.Token(lex.TokenType.end, "END")

    def NextToken(self):
        self.NextTokenSkipWS()
        while (self.crtToken.type == lex.TokenType.whitespace):
            #print("--> Skipping WS")
            self.NextTokenSkipWS()

        #print("Next Token Set to ::: ", self.crtToken.type, self.crtToken.lexeme)                 
    def _validate_type_match(self, declared_type, value_node):
        """Verify that the value matches the declared type"""
        # ... (existing let and colon checks remain the same)
        
        # Get the actual type of the value
        actual_type = self._get_expression_type(value_node)
        
        # Check for missing type
        if declared_type == None:
            raise SyntaxError(f"Missing Type")
        if declared_type != actual_type:
            # Handle special cases (like int vs float)
            if  (declared_type != actual_type):  # Allow int->float
                raise SyntaxError(f"Type mismatch: cannot assign {declared_type} to {actual_type} variable")

    def _get_expression_type(self, node):
        """Determine the type of an expression node"""
        if isinstance(node, ast.ASTIntegerNode):
            return "int"
        elif isinstance(node, ast.ASTFloatNode):
            return "float"
        elif isinstance(node, ast.ASTBooleanNode):
            return "boolean"
        elif isinstance(node, ast.ASTColourNode):
            return "colour"
        elif isinstance(node, ast.ASTMultiOpNode):
            # For binary ops, return the "dominant" type
            left_type = self._get_expression_type(node.left)
            right_type = self._get_expression_type(node.right)
            if "float" in [left_type, right_type]:
                return "float"
            return left_type  # default to left type
    
        elif isinstance(node, ast.ASTExpressionNode):
            if hasattr(node, 'left') and hasattr(node, 'right'):
                left_type = self._get_expression_type(node.left)
                right_type = self._get_expression_type(node.right)
                if "float" in [left_type, right_type]:
                    return "float"
                return left_type
            elif hasattr(node, 'right'):  # for unary expressions like -x
                return self._get_expression_type(node.right)
        else:
            return "unknown"
    
    def ParseSimpleExpr(self):
        left = self.ParseTerm()

        while self.crtToken.type == lex.TokenType.addop:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseTerm()
            left = ast.ASTExpressionNode(op=op, left=left, right=right)

        return left

    def ParseExpression(self):
        left = self.ParseSimpleExpr()

        # Handle relational ops (==, !=, <, >, etc.)
        while self.crtToken.type == lex.TokenType.relop:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseSimpleExpr()
            left = ast.ASTExpressionNode(op=op, left=left, right=right)

        # Handle optional 'as' cast
        if self.crtToken.type == lex.TokenType.as_kw:
            self.NextToken()
            if self.crtToken.type != lex.TokenType.type:
                raise SyntaxError(f"Expected type after 'as', got {self.crtToken.lexeme}")
            cast_type = self.crtToken.lexeme
            self.NextToken()
            left.cast_type = cast_type  # store cast type in AST node if needed

        return left

    def ParseTerm(self):
        left = self.ParseFactor()

        if self.crtToken.type != lex.TokenType.mulop:
            raise SyntaxError(f"Expected multiplicative operator after factor, got '{self.crtToken.lexeme}'")

        while self.crtToken.type == lex.TokenType.mulop:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseFactor()
            left = ast.ASTExpressionNode(op=op, left=left, right=right)

        return left



    def ParseFactor(self):
        token = self.crtToken
        # Literal (int/float/boolean/colour)
        if token.type == lex.TokenType.integerliteral:
            value = token.lexeme
            self.NextToken()
            return ast.ASTIntegerNode(value)

        elif token.type == lex.TokenType.floatliteral:
            value = token.lexeme
            self.NextToken()
            return ast.ASTFloatNode(value)

        elif token.type == lex.TokenType.booleanliteral:
            value = token.lexeme
            self.NextToken()
            return ast.ASTBooleanNode(value)

        elif token.type == lex.TokenType.colourliteral:
            value = token.lexeme
            self.NextToken()
            return ast.ASTColourNode(value)

        # Identifier
        elif token.type == lex.TokenType.identifier:
            value = token.lexeme
            self.NextToken()
            return ast.ASTVariableNode(value)

        # Pad functions
        # elif token.type == lex.TokenType.padread:
        #     self.NextToken()
        #     return ast.ASTPadReadNode()

        # elif token.type == lex.TokenType.padrandi:
        #     self.NextToken()
        #     return ast.ASTPadRandINode()

        # elif token.type == lex.TokenType.padwidth:
        #     self.NextToken()
        #     return ast.ASTPadWidthNode()

        # elif token.type == lex.TokenType.padheight:
        #     self.NextToken()
        #     return ast.ASTPadHeightNode()

        # # Subexpression (parentheses)
        # elif token.type == lex.TokenType.lparen:
        #     self.NextToken()
        #     expr = self.ParseExpression()
        #     if self.crtToken.type != lex.TokenType.rparen:
        #         raise SyntaxError("Expected ')' after expression")
        #     self.NextToken()
        #     return expr

        # Unary negation (e.g. -x)
        elif token.type == lex.TokenType.addop and token.lexeme == '-':
            self.NextToken()
            right = self.ParseFactor()
            return ast.ASTExpressionNode(op='neg', right=right)

        raise SyntaxError(f"Unexpected token in expression: {token.lexeme}")

    def ParseDeclaration(self):
        # 1. Expect 'let'
        if self.crtToken.type != lex.TokenType.let:
            raise SyntaxError(f"Expected 'let' at start of declaration, got {self.crtToken.lexeme}")
        self.NextToken()

        # 2. Expect a type
        if self.crtToken.type != lex.TokenType.type:
            raise SyntaxError(f"Expected type after 'let', got {self.crtToken.lexeme}")
        declaration_type = ast.ASTTypeNode(self.crtToken.lexeme)
        dec_type = self.crtToken.lexeme
        self.NextToken()

        # 3. Expect colon ':'
        if self.crtToken.type != lex.TokenType.colon:
            raise SyntaxError(f"Expected ':' after type, got {self.crtToken.lexeme}")
        self.NextToken()

        # 4. Expect identifier (variable name)
        if self.crtToken.type != lex.TokenType.identifier:
            raise SyntaxError(f"Expected identifier after ':', got {self.crtToken.lexeme}")
        declaration_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
        self.NextToken()

        # 5. Expect '='
        if self.crtToken.type != lex.TokenType.equals:
            raise SyntaxError(f"Expected '=' after variable name, got {self.crtToken.lexeme}")
        self.NextToken()

        # 6. Parse the right-hand side expression
        declaration_rhs = self.ParseExpression()

        # 7. Type checking (assuming you want this at parse-time)
        self._validate_type_match(dec_type, declaration_rhs)

        # 8. Build and return AST node
        return ast.ASTDeclarationNode(declaration_type, declaration_lhs, declaration_rhs)

            
    def ParseStatement(self):
        #At the moment we only have assignment statements .... you'll need to add more for the assignment - branching depends on the token type
        return self.ParseDeclaration()


    def ParseProgram(self):                        
        self.NextToken()  # set crtToken to the first token (skip all WS)
        program = ast.ASTProgramNode()  # Create program node
        
        # Parse all statements until end of input
        while self.crtToken.type != lex.TokenType.end:
            # Handle semicolon between statements
            if self.crtToken.type == lex.TokenType.semicolon:
                self.NextToken()
                continue
                
            # Parse the statement and add to program
            s = self.ParseStatement()
            program.add_statement(s)
            
            # Expect semicolon after statements (unless it's the last statement)
            if self.crtToken.type != lex.TokenType.end and self.crtToken.type != lex.TokenType.semicolon:
                print("Syntax Error - No Semicolon separating statements")
                break
                
            if self.crtToken.type == lex.TokenType.semicolon:
                self.NextToken()
        
        return program

    def Parse(self):        
        self.ASTroot = self.ParseProgram()


#parser = Parser("x=23;")
parser = Parser("  let float  :   x=  3.2 * 4.3;")
parser.Parse()

print_visitor = ast.PrintNodesVisitor()
parser.ASTroot.accept(print_visitor)