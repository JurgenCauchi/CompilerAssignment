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
        self.ASTroot = None   

    def IsStartOfSimpleExpression(self):
        # Tokens that can start an expression:
        return self.crtToken.type in (
            lex.TokenType.identifier,    # e.g., 'x', 'foo'
            lex.TokenType.integerliteral,        # e.g., '42'
            lex.TokenType.floatliteral,        # e.g., '"3.4"'
            lex.TokenType.colourliteral, 
            lex.TokenType.booleanliteral,
            lex.TokenType.not_kw,# e.g., '"hello"       # e.g., '"hello"'
            lex.TokenType.padheight,
            lex.TokenType.padwidth,
            lex.TokenType.padread, 
            lex.TokenType.padrandom_int, 
            # Add other expression-starting tokens (e.g., 'PadRandI')
        )
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
        if isinstance(node, ast.ASTIntegerNode) or isinstance(node,ast.ASTPadHeightNode) or isinstance(node,ast.ASTPadWidthNode) or isinstance(node,ast.ASTPadRandINode):
            return "int"
        elif isinstance(node, ast.ASTFloatNode):
            return "float"
        elif isinstance(node, ast.ASTBooleanNode):
            return "bool"
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

        while self.crtToken.type == lex.TokenType.addop or self.crtToken.type == lex.TokenType.minus:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseTerm()
            left = ast.ASTAddOpNode(op=op, left=left, right=right)

        return left

    def ParseFormalParam(self):
        intlit = None
        ident = self.crtToken.lexeme
        self.NextToken()

        if self.crtToken.type != lex.TokenType.colon:
            raise SyntaxError(f"Expected : after identifier, got {self.crtToken.lexeme}")
        self.NextToken()


        if self.crtToken.type != lex.TokenType.type:
            raise SyntaxError(f"Expected type, got {self.crtToken.lexeme}")
        type = self.crtToken.lexeme
        self.NextToken()

        if self.crtToken.type == lex.TokenType.lsqr:
            self.NextToken()
            if self.crtToken.type != lex.TokenType.integerliteral:
                raise SyntaxError(f"Expected integer literal, got {self.crtToken.lexeme}")
            intlit = self.crtToken.lexeme
            self.NextToken()
            if self.crtToken.type != lex.TokenType.rsqr:
                raise SyntaxError(f"Expected ] after identifier, got {self.crtToken.lexeme}")

        return (ident,type,intlit)
    
    def ParseFormalParams(self):
        params = []
        # Parse first parameter
        params.append(self.ParseFormalParam())

        # Parse additional parameters separated by commas
        while self.crtToken.type == lex.TokenType.comma:
            self.NextToken()  # consume comma
            params.append(self.ParseFormalParam())

        return params
    
    def ParseFuncDecl(self):
        formalparam = None
        intlit = None
        self.NextToken()
        if self.crtToken.type != lex.TokenType.identifier:
            raise SyntaxError(f"Expected identifier got {self.crtToken.lexeme}")
        ident = self.crtToken.lexeme
        self.NextToken()
        if self.crtToken.type != lex.TokenType.lparen:
            raise SyntaxError(f"Expected ( after identifier, got {self.crtToken.lexeme}")
        self.NextToken()

        if self.crtToken.type == lex.TokenType.identifier:
            formalparam = self.ParseFormalParams()
            


        if self.crtToken.type != lex.TokenType.rparen:
            raise SyntaxError(f"Expected ) after, got {self.crtToken.lexeme}")
        self.NextToken()
    
        if self.crtToken.type != lex.TokenType.arrow:
            raise SyntaxError(f"Expected -> after ), got {self.crtToken.lexeme}")
        
        self.NextToken()

        if self.crtToken.type != lex.TokenType.type:
            raise SyntaxError(f"Expected type, got {self.crtToken.lexeme}")
        type = self.crtToken.lexeme

        
        self.NextToken()

        if self.crtToken.type == lex.TokenType.lsqr:
            self.NextToken()
            if self.crtToken.type != lex.TokenType.integerliteral:
                raise SyntaxError(f"Expected integer literal, got {self.crtToken.lexeme}")
            intlit = self.crtToken.lexeme
            self.NextToken()
            if self.crtToken.type != lex.TokenType.rsqr:
                raise SyntaxError(f"Expected ] after identifier, got {self.crtToken.lexeme}")
            self.NextToken()
        block = self.ParseBlock()

        return ast.ASTFunctionDeclNode(ident,formalparam,type,intlit,block)

    def ParseExpression(self):
        left = self.ParseSimpleExpr()

        # Handle relational ops (==, !=, <, >, etc.)
        while self.crtToken.type == lex.TokenType.relop:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseSimpleExpr()
            left = ast.ASTRelOpNode(op=op, left=left, right=right)
        
        
        # Handle optional 'as' cast
        if self.crtToken.type == lex.TokenType.as_kw:
            self.NextToken()
            if self.crtToken.type != lex.TokenType.type:
                raise SyntaxError(f"Expected type after 'as', got {self.crtToken.lexeme}")
            cast_type = self.crtToken.lexeme
            self.NextToken()
            left = ast.ASTCastNode(expr=left, to_type=cast_type)

        return left

    def ParseTerm(self):
        left = self.ParseFactor()

        # Optional multiplicative operators
        while self.crtToken.type == lex.TokenType.mulop:
            op = self.crtToken.lexeme
            self.NextToken()
            right = self.ParseFactor()
            left = ast.ASTMultiOpNode(op=op, left=left, right=right)

        return left
    
    def ParseUnary(self):
        token = self.crtToken
        if token.type == lex.TokenType.minus or token.type == lex.TokenType.not_kw:
            op = self.crtToken.lexeme
            right = self.ParseExpression()  # or parse the appropriate precedence level
            return ast.ASTUnaryNode(op,right)



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

        #Pad functions
        elif token.type == lex.TokenType.padread:
            self.NextToken()
            exprs = self.ParseRead()
            return exprs

        elif token.type == lex.TokenType.padrandom_int:
            self.NextToken()
            expr = self.ParseRandInt()
            return expr

        elif token.type == lex.TokenType.padwidth:
            self.NextToken()
            return ast.ASTPadWidthNode()

        elif token.type == lex.TokenType.padheight:
            self.NextToken()
            return ast.ASTPadHeightNode()

        # Subexpression (parentheses)
        elif token.type == lex.TokenType.lparen:
            self.NextToken()
            expr = self.ParseExpression()
            if self.crtToken.type != lex.TokenType.rparen:
                raise SyntaxError("Expected ')' after expression")
            self.NextToken()
            return expr

        # Unary negation (e.g. -x)
        elif token.type == lex.TokenType.not_kw or token.type == lex.TokenType.minus:
            self.NextToken()
            right = self.ParseExpression()
            return ast.ASTExpressionNode(op='neg', right=right)

    def ParseReturn(self):
        returntypes = []
        self.NextToken()
        expr = self.ParseExpression()
        returntypes.append(self._get_expression_type(expr))
        #print(returntypes)
        return ast.ASTReturnNode(expr,returntypes)
    
    def ParsePrint(self):
        self.NextToken()
        expr = self.ParseExpression()
        return ast.ASTPrintNode(expr)

    def ParseDelay(self):
        if self.crtToken.type != lex.TokenType.delay:
            raise SyntaxError(f"Expected '__delay' at start of the print statement, got {self.crtToken.lexeme}")
        self.NextToken()
        expr = self.ParseExpression()
        return ast.ASTDelayNode(expr)
    

    def ParseForloop(self):
        self.NextToken()
        if self.crtToken.type != lex.TokenType.lparen:
            raise SyntaxError(f"Expected '(' after for , got {self.crtToken.lexeme}")
        
        self.NextToken()
        vardec = self.ParseDeclaration()

        if self.crtToken.type != lex.TokenType.semicolon:
            raise SyntaxError(f"Expected ';' after declaring variable , got {self.crtToken.lexeme}")
        
        self.NextToken()
        expr = self.ParseExpression()

        if self.crtToken.type != lex.TokenType.semicolon:
            raise SyntaxError(f"Expected ';' after expression , got {self.crtToken.lexeme}")

        self.NextToken()
        assign = self.ParseAssignment()
        
        if self.crtToken.type != lex.TokenType.rparen:
            raise SyntaxError(f"Expected ')' after assignming  , got {self.crtToken.lexeme}")
        
        self.NextToken()
        block = self.ParseBlock()

        return ast.ASTForNode(vardec,expr,assign,block)
    

    def ParseIf(self):
        blocks = []
        self.NextToken()
        if self.crtToken.type != lex.TokenType.lparen:
            raise SyntaxError(f"Expected '(' after if, got {self.crtToken.lexeme}")
        
        self.NextToken()
        expr = self.ParseExpression()

        if self.crtToken.type != lex.TokenType.rparen:
            raise SyntaxError(f"Expected ')' after expression, got {self.crtToken.lexeme}")
        
        self.NextToken()
        blocks.append(self.ParseBlock())

        if self.crtToken.type == lex.TokenType.else_kw:
            self.NextToken()  # Consume the else keyword
            blocks.append(self.ParseBlock())

        return ast.ASTIfNode(expr, blocks)
    

    def ParseWhile(self):
        self.NextToken()
        if self.crtToken.type != lex.TokenType.lparen:
            raise SyntaxError(f"Expected '(' after if, got {self.crtToken.lexeme}")
        
        self.NextToken()
        expr = self.ParseExpression()
        
        if self.crtToken.type != lex.TokenType.rparen:
            raise SyntaxError(f"Expected ')' after expression, got {self.crtToken.lexeme}")
        
        self.NextToken()
        block = self.ParseBlock()

        return ast.ASTWhileNode(expr, block)
    

    def ParseRead(self):
        expressions = []
        expressions.append(self.ParseExpression())
        
        if self.crtToken.type != lex.TokenType.comma:
            raise SyntaxError(f"Expected ',' after expression, got {self.crtToken.lexeme}")
        
        self.NextToken()

        expressions.append(self.ParseExpression())

        return ast.ASTPadReadNode(expressions)
    
    def ParseRandInt(self):
        expression = self.ParseExpression()
        self._validate_type_match("int",expression)
        return ast.ASTPadRandINode(expression)
        
        

    def ParseWrite(self):
        expressions = []
        if self.crtToken.type == lex.TokenType.wrbox:
            kw = 0
        elif self.crtToken.type == lex.TokenType.write:
            kw = 1
            # Parse first expression (required)
        self.NextToken()
 
        expr = self.ParseExpression()
        expressions.append(expr)
        
            # Parse additional comma-separated expressions
        while self.crtToken.type == lex.TokenType.comma:
            self.NextToken()
            expr = self.ParseExpression()
            expressions.append(expr)
            # Validate minimum number of expressions
        size = len(expressions)
        return ast.ASTWriteNode(expressions,kw,size)
    


    def ParseBlock(self):
        if self.crtToken.type != lex.TokenType.lcurly:
            raise SyntaxError(f"Expected '{{', got {self.crtToken.lexeme}")
        self.NextToken()
        
        block = ast.ASTBlockNode()
        while self.crtToken.type != lex.TokenType.rcurly:        
            stmt = self.ParseStatement()
            if stmt is not None:  # Optional: Skip None
                block.add_statement(stmt)

        if self.crtToken.type != lex.TokenType.rcurly:
            raise SyntaxError(f"Expected '}}', got {self.crtToken.lexeme}")


        self.NextToken()

    
        return block 
    
    def ParseDeclaration(self):
        # 1. Expect 'let'
        if self.crtToken.type != lex.TokenType.let:
            raise SyntaxError(f"Expected 'let' at start of declaration, got {self.crtToken.lexeme}")
        self.NextToken()

        # 2. Expect a type
        if self.crtToken.type != lex.TokenType.identifier:
            raise SyntaxError(f"Expected identifier after 'let', got {self.crtToken.lexeme}")
        declaration_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
        self.NextToken()

       
        # 3. Expect colon ':'
        if self.crtToken.type != lex.TokenType.colon:
            raise SyntaxError(f"Expected ':' after type, got {self.crtToken.lexeme}")
        self.NextToken()

        # 4. Expect type(variable name)
        if self.crtToken.type != lex.TokenType.type:
            raise SyntaxError(f"Expected type after ':', got {self.crtToken.lexeme}")
        declaration_type = ast.ASTTypeNode(self.crtToken.lexeme)
        dec_type = self.crtToken.lexeme
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
    
    def ParseAssignment(self):

        # 1. Expect identifier (variable name)
        if self.crtToken.type != lex.TokenType.identifier:
            raise SyntaxError(f"Expected identifier after, got {self.crtToken.lexeme}")
        assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
        self.NextToken()

        # 2. Expect '='
        if self.crtToken.type != lex.TokenType.equals:
            raise SyntaxError(f"Expected '=' after variable name, got {self.crtToken.lexeme}")
        self.NextToken()

        # 3. Parse the right-hand side expression
        assignment_rhs = self.ParseExpression()


        # 4. Build and return AST node
        return ast.ASTAssignmentNode(assignment_lhs, assignment_rhs)
            
    def ParseStatement(self):
        stmt = None
        if self.crtToken.type == lex.TokenType.let:
            stmt = self.ParseDeclaration()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type == lex.TokenType.rtrn:
            stmt = self.ParseReturn()
            #self.NextToken()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type == lex.TokenType.print:
            stmt = self.ParsePrint()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type == lex.TokenType.delay:
            stmt = self.ParseDelay()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type in (lex.TokenType.write, lex.TokenType.wrbox):
            stmt = self.ParseWrite()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type == lex.TokenType.for_kw:
            stmt = self.ParseForloop()
            return stmt  # Block statement - no semicolon needed
        elif self.crtToken.type == lex.TokenType.identifier:
            stmt = self.ParseAssignment()
            if self.crtToken.type != lex.TokenType.semicolon:
                raise SyntaxError(f"Expected ; got: {self.crtToken.type}")
            self.NextToken()
        elif self.crtToken.type == lex.TokenType.if_kw:
            stmt = self.ParseIf()
        elif self.crtToken.type == lex.TokenType.while_kw:
            stmt = self.ParseWhile()
        elif self.crtToken.type == lex.TokenType.fun:
            stmt = self.ParseFuncDecl()
        elif self.crtToken.type == lex.TokenType.lcurly:
            stmt = self.ParseBlock()
        else:
            raise SyntaxError(f"Unexpected token: {self.crtToken.type}")
        
        #self.NextToken()  # Consume the semicolon
        return stmt

    def ParseProgram(self):                        
        self.NextToken()  # set crtToken to the first token (skip all WS)
        program = ast.ASTProgramNode()  # Create program node
        
        # Parse all statements until end of input
        while self.crtToken.type != lex.TokenType.end:
            # Skip any extra semicolons between statements
            # while self.crtToken.type == lex.TokenType.semicolon:
            #     self.NextToken()
            #     continue
                
            # Parse the statement
            stmt = self.ParseStatement()
            program.add_statement(stmt)
            
            # Only enforce semicolon if:
            # 1. Not at end of input
            # 2. Not a block statement (for/if/while)
            # 3. Next token isn't already a semicolon
            # if (self.crtToken.type != lex.TokenType.end and 
            #     not isinstance(stmt, (ast.ASTForNode, ast.ASTBlockNode,ast.ASTIfNode,ast.ASTFunctionDeclNode)) and
            #     self.crtToken.type != lex.TokenType.semicolon):
            #     raise SyntaxError(f"Expected ';' after statement, got {self.crtToken.lexeme}")
            
            # Consume the semicolon if present
            if self.crtToken.type == lex.TokenType.semicolon:
                self.NextToken()
        
        return program

    def Parse(self):        
        self.ASTroot = self.ParseProgram()
        return self.ASTroot  

#parser = Parser(" not 1 * 2 > 3 * 4 * 5 * 6 > 7 * 8 > not 9 * 10 > 11 * 12 * 13 * 14 > 15 * 16 ")
#parser = Parser("for (let int:x = 5; 3 > 5; x = 3) {  x = 2; } let int:x = 2;")

# parser = Parser(""" 

#     fun Race(p1_c:colour, p2_c:colour, score_max:int) -> int {
#         let p1_score:int = 0;
#         let p2_score:int = 0;


#         while ((p1_score < score_max) and (p2_score < score_max)) {
#             let p1_toss:int = __randi 1000;
#             let p2_toss:int = __randi 1000;

#             if (p1_toss > p2_toss) {
#             p1_score = p1_score + 1;
#             __write 1, p1_score, p1_c;
#             } else {
#                 p2_score = p2_score + 1;
#                 __write 2, p2_score, p2_c;
#             }

#             __delay 100;
#  }

#  if (p2_score > p1_score) {
#  return 2;
#  }

#  return 1;
#  }



#                 """)


parser = Parser("""
                
                __write 5,3,3,3,3,3,3;
                """)

parser.Parse()

print_visitor = ast.PrintNodesVisitor()
parser.ASTroot.accept(print_visitor)