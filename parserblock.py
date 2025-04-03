# Now we need the parser (using tokens produced by the Lexer) to build the AST - this code snipper is able to build ASTAssignmentNode trees. LHS can only be an integer here ....
# A small predictive recursive descent parser
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
        self.ASTroot = ast.ASTAssignmentNode     #this will need to change once you introduce the AST program node .... that should become the new root node    

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

        
    def _validate_type_match(self, let_node, declared_type, value_node, colon_node):
        """Verify that the value matches the declared type"""
        # Get the actual type of the value
        letcheck = None
        coloncheck = None

        if isinstance(colon_node, ast.ASTColonNode):
            coloncheck = ":"


        if isinstance(let_node, ast.ASTLetNode):
            letcheck = "let"
        
        if isinstance(value_node, ast.ASTIntegerNode):
            actual_type = "int"
        elif isinstance(value_node, ast.ASTFloatNode):
            actual_type = "float"
        elif isinstance(value_node, ast.ASTBooleanNode):
            actual_type = "boolean"
        elif isinstance(value_node, ast.ASTColourNode):
            actual_type = "colour"
        else:
            actual_type = "unknown"

        if letcheck != "let":

            raise SyntaxError(f"Missing let")

        if coloncheck == None:
            print(coloncheck)
            raise SyntaxError(f"Missing Colon")
        
        # Check for missing type
        if declared_type == None:
            raise SyntaxError(f"Missing Type")
        if declared_type != actual_type:
                # Handle special cases (like int vs float)
            raise SyntaxError(f"Type mismatch: cannot assign {actual_type} to {declared_type} variable")

            
    def ParseExpression(self):

        if (self.crtToken.type == lex.TokenType.integerliteral):
            value = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTIntegerNode(value)
        if (self.crtToken.type == lex.TokenType.floatliteral):
            value = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTFloatNode(value)
        if (self.crtToken.type == lex.TokenType.colourliteral):
            value = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTColourNode(value)
        if (self.crtToken.type == lex.TokenType.booleanliteral):
            value = self.crtToken.lexeme
            self.NextToken()
            return ast.ASTBooleanNode(value)

    def ParseAssignment(self):
        ass_type = None
        assignment_colon = None
        assignment_let = None
        if (self.crtToken.type == lex.TokenType.let):
            assignment_let = ast.ASTLetNode(self.crtToken.lexeme)
            self.NextToken() 
        if (self.crtToken.type == lex.TokenType.type):
            assignment_type = ast.ASTTypeNode(self.crtToken.lexeme)
            ass_type = self.crtToken.lexeme
            self.NextToken()
        if (self.crtToken.type == lex.TokenType.colon):
            assignment_colon = ast.ASTColonNode(self.crtToken.lexeme)
            self.NextToken()
        #Assignment is made up of two main parts; the LHS (the variable) and RHS (the expression)
        if (self.crtToken.type == lex.TokenType.identifier):
            #create AST node to store the identifier            
            assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.NextToken()
            #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

        if (self.crtToken.type == lex.TokenType.equals):
            self.NextToken()

 
        #Next sequence of tokens should make up an expression ... therefor call ParseExpression that will return the subtree representing that expression
        assignment_rhs = self.ParseExpression()
        
        
        self._validate_type_match(assignment_let, ass_type, assignment_rhs, assignment_colon)

        return ast.ASTAssignmentNode(assignment_let, assignment_type, assignment_colon, assignment_lhs, assignment_rhs)
            
    def ParseStatement(self):
        #At the moment we only have assignment statements .... you'll need to add more for the assignment - branching depends on the token type
        return self.ParseAssignment()

    def ParseBlock(self):
        #At the moment we only have assignment statements .... you'll need to add more for the assignment - branching depends on the token type

        block = ast.ASTBlockNode()

        while (self.crtToken.type != lex.TokenType.end):
            #print("New Statement - Processing Initial Token:: ", self.crtToken.type, self.crtToken.lexeme)
            s = self.ParseStatement()
            block.add_statement(s)
            if (self.crtToken.type == lex.TokenType.semicolon):
                self.NextToken()
            else:
                print("Syntax Error - No Semicolon separating statements in block")
                break
        
        return block

    def ParseProgram(self):                        
        self.NextToken()  #set crtToken to the first token (skip all WS)
        b = self.ParseBlock()        
        return b        

    def Parse(self):        
        self.ASTroot = self.ParseProgram()


#parser = Parser("x=23;")
parser = Parser("  let float  :   x=   3.4 ;")
parser.Parse()

print_visitor = ast.PrintNodesVisitor()
parser.ASTroot.accept(print_visitor)