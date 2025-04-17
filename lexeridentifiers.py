# Import Enum class from enum module to create enumerated constants
from enum import Enum

# Define different types of tokens our lexer will recognize
class TokenType(Enum):
    let =           1
    rtrn =          2
    for_kw =        3
    while_kw =      4
    if_kw =         5
    else_kw =       6
    fun =           7 
    identifier =    8    # For variable names, function names, etc.
    whitespace =    9    # For spaces, tabs, etc.
    booleanliteral= 10        # For values
    integerliteral= 11
    floatliteral =  12
    colourliteral = 13
    comma =         14     # For seperators/punctuator
    mulop =         15
    addop =         16
    relop =         17
    semicolon =     18
    colon =         19
    equals =        20
    type =          21
    as_kw =         22
    padwidth =      23
    padheight =     24
    padread =       25
    padrandom_int = 26
    print =         27
    delay =         28
    wrbox =         29
    write =         30
    lcurly =        31
    rcurly =        32
    lparen =        33
    rparen =        34
    not_kw =        35
    lsqr =          36
    rsqr =          37
    arrow =         38
    minus =         39
    error =         40       # For invalid tokens
    end =           41        # For end of input

# Class to represent a token with its type and actual text (lexeme)
class Token:
    def __init__(self, t, l):
        self.type = t  # Token type (from TokenType enum)
        self.lexeme = l  # The actual text of the token

# Main lexer class that converts source code into tokens
class Lexer:
    def __init__(self):
        # Categories of characters we'll encounter
        self.lexeme_list = ["_","-",">", ".", "#",";",":", "=","[","]","comma", "letter", "digit","{","}","(",")","mulop","addop","relop","boolean_value", "ws", "other"]

        self.type_list = {"int", "float" , "bool" , "colour" }
        
        self.boolean_list = {"true", "false"}
        
        # Possible states of our finite automaton
        self.states_list = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 , 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 , 21 , 22,23]
        
        # Which states are accepting (valid end states for tokens)
        self.states_accp = [1, 2, 3, 4, 5, 7, 9 , 10, 11 ,12, 13, 14, 15, 16, 17, 18, 19 , 20, 21 , 22 ,23]

        # Calculate dimensions for our transition table
        self.rows = len(self.states_list)
        self.cols = len(self.lexeme_list)

        # Initialize transition table with -1 (error state)
        # Tx[state][character_category] = next_state
        self.Tx = [[-1 for j in range(self.cols)] for i in range(self.rows)]
        
        # Set up the transition rules
        self.InitialiseTxTable()     

    def InitialiseTxTable(self):
        # Define the state transitions for our lexer's finite automaton
        
        # From state 0 (start state):
        # - On letter or underscore, go to state 1 (identifier)
        self.Tx[0][self.lexeme_list.index("letter")] = 1
        self.Tx[0][self.lexeme_list.index("_")] = 1
        self.Tx[1][self.lexeme_list.index("_")] = 1
        
        # From state 1 (identifier):
        # - On letter or digit, stay in state 1
        self.Tx[1][self.lexeme_list.index("letter")] = 1
        self.Tx[1][self.lexeme_list.index("digit")] = 1

        # White space handling:
        # - From state 0, on whitespace go to state 2
        # - From state 2, on whitespace stay in state 2
        self.Tx[0][self.lexeme_list.index("ws")] = 2
        self.Tx[2][self.lexeme_list.index("ws")] = 2

        # - From state 0, on digit go to state 3
        # - From state 3, on digit stay in state 3
        self.Tx[0][self.lexeme_list.index("digit")] = 3
        self.Tx[3][self.lexeme_list.index("digit")] = 3
        self.Tx[3][self.lexeme_list.index(".")] = 6
        self.Tx[6][self.lexeme_list.index("digit")] = 7
        self.Tx[7][self.lexeme_list.index("digit")] = 7

        self.Tx[0][self.lexeme_list.index("boolean_value")] = 3

        # - From state 0, on seperator go to state 4
        self.Tx[0][self.lexeme_list.index("comma")] = 4

        self.Tx[0][self.lexeme_list.index("mulop")] = 5

        self.Tx[0][self.lexeme_list.index("addop")] = 13 

        self.Tx[0][self.lexeme_list.index("relop")] = 14
        self.Tx[0][self.lexeme_list.index(">")] = 14

        self.Tx[14][self.lexeme_list.index("=")] = 15 

        # - From state 0, on # go to state 8
        self.Tx[0][self.lexeme_list.index("#")] = 8

        # When theres a digit, letter move to the next state
        self.Tx[8][self.lexeme_list.index("digit")] = 9
        self.Tx[8][self.lexeme_list.index("letter")] = 9

        self.Tx[9][self.lexeme_list.index("digit")] = 9
        self.Tx[9][self.lexeme_list.index("letter")] = 9

        self.Tx[0][self.lexeme_list.index(";")] = 10

        self.Tx[0][self.lexeme_list.index("=")] = 11
        self.Tx[11][self.lexeme_list.index("=")] = 15

        self.Tx[0][self.lexeme_list.index(":")] = 12

        #To get the curly brackets token
        self.Tx[0][self.lexeme_list.index("{")] = 16
        self.Tx[0][self.lexeme_list.index("}")] = 17

        #To get the brackets token
        self.Tx[0][self.lexeme_list.index("(")] = 18
        self.Tx[0][self.lexeme_list.index(")")] = 19

        self.Tx[0][self.lexeme_list.index("[")] = 20
        self.Tx[0][self.lexeme_list.index("]")] = 21

        self.Tx[0][self.lexeme_list.index("-")] = 22
        self.Tx[22][self.lexeme_list.index(">")] = 23

  
        # # Print the transition table for debugging
        # for row in self.Tx:
        #     print(row)

    # Check if a given state is an accepting state
    def AcceptingStates(self, state):
        try:
            self.states_accp.index(state)
            return True
        except ValueError:
            return False

    # Determine what kind of token we've found based on final state
    def GetTokenTypeByFinalState(self, state, lexeme):
        if state == 1:  # Identifier state
            # Check if lexeme is in keyword list first
            if lexeme in self.boolean_list: 
                return Token(TokenType.booleanliteral, lexeme)
            elif lexeme in self.type_list:
                return Token(TokenType.type, lexeme)
            elif lexeme == "and":
                return Token(TokenType.mulop, lexeme)
            elif lexeme == "or":
                return Token(TokenType.addop, lexeme)
            elif lexeme == "return":
                return Token(TokenType.rtrn, lexeme)
            elif lexeme == "for":
                return Token(TokenType.for_kw, lexeme)
            elif lexeme == "while":
                return Token(TokenType.while_kw, lexeme)
            elif lexeme == "if":
                return Token(TokenType.if_kw, lexeme)
            elif lexeme == "else":
                return Token(TokenType.else_kw, lexeme)
            elif lexeme == "fun":
                return Token(TokenType.fun, lexeme)
            elif lexeme == "let":
                return Token(TokenType.let, lexeme)
            elif lexeme == "as":
                return Token(TokenType.as_kw, lexeme)
            elif lexeme == "__width":
                return Token(TokenType.padwidth, lexeme)
            elif lexeme == "__height":
                return Token(TokenType.padheight, lexeme)
            elif lexeme == "__read":
                return Token(TokenType.padread, lexeme)
            elif lexeme == "__random_int":
                return Token(TokenType.padrandom_int, lexeme)
            elif lexeme == "not":
                return Token(TokenType.not_kw, lexeme)
            elif lexeme == "__print":
                return Token(TokenType.print, lexeme)
            elif lexeme == "__delay":
                return Token(TokenType.delay, lexeme)
            elif lexeme == "__write_box":
                return Token(TokenType.wrbox, lexeme)
            elif lexeme == "__write":
                return Token(TokenType.write, lexeme)
            else:
                return Token(TokenType.identifier, lexeme)
        elif state == 2:  # Whitespace state
            return Token(TokenType.whitespace, lexeme)
        elif state == 3:
            return Token(TokenType.integerliteral, lexeme )
        elif state == 4:
            return Token(TokenType.comma, lexeme)
        elif state == 5:
            return Token(TokenType.mulop, lexeme)
        elif state == 7: 
            return Token(TokenType.floatliteral, lexeme)
        elif state == 9:             
            if len(lexeme) != 7 or not all(c.lower() in '0123456789abcdef' for c in lexeme[1:]):
                return Token(TokenType.error, lexeme)
            else:
                return Token(TokenType.colourliteral, lexeme)
        elif state == 10:
            return Token(TokenType.semicolon, lexeme)
        elif state == 11:
            return Token(TokenType.equals, lexeme)
        elif state == 12:
            return Token(TokenType.colon, lexeme)
        elif state == 13:
            return Token(TokenType.addop, lexeme)
        elif state == 14:
            return Token(TokenType.relop, lexeme)
        elif state == 15:
            return Token(TokenType.relop, lexeme)
        elif state == 16:
            return Token(TokenType.lcurly, lexeme)
        elif state == 17:
            return Token(TokenType.rcurly, lexeme)
        elif state == 18:
            return Token(TokenType.lparen, lexeme)
        elif state == 19:
            return Token(TokenType.rparen, lexeme)
        elif state == 20:
            return Token(TokenType.lsqr, lexeme)
        elif state == 21:
            return Token(TokenType.rsqr, lexeme)
        elif state == 22:
            return Token(TokenType.minus, lexeme)
        elif state == 23:
            return Token(TokenType.arrow, lexeme)
        else:
            return 'default result'
        

    # Categorize a character into one of our lexeme types
    def CatChar(self, character):
        cat = "other"  # Default category
        if character.isalpha(): cat = "letter"
        if character.isdigit(): cat = "digit"
        if character == "_": cat = "_"
        if character == ".": cat = "."  
        if character == "\"": cat = "\""
        if character == "#": cat = "#"
        if character.isspace(): cat = "ws"      
        if character == ";": cat = ";"
        if character == ":": cat = ":"
        if character == "=": cat = "="
        if character == "-": cat = "-"
        if character == ">": cat = ">"
        if character == "}" : cat = "}"
        if character == "{" : cat = "{"
        if character == "(" : cat = "("
        if character == ")" : cat = ")"
        if character in {"*","/",}: cat = "mulop"
        if character == "+": cat = "addop"
        if character in {"<","!"}: cat = "relop"
        if character == ",": cat = "comma"
        return cat

    # Check if we've reached the end of input
    def EndOfInput(self, src_program_str, src_program_idx):
        return src_program_idx > len(src_program_str)-1

    # Get the next character from input, handling end-of-input
    def NextChar(self, src_program_str, src_program_idx):
        if not self.EndOfInput(src_program_str, src_program_idx):
            return True, src_program_str[src_program_idx]
        else: 
            return False, "."  # Return dummy character at end

    # Main tokenization function - gets the next token from input
    def NextToken(self, src_program_str, src_program_idx):
        state = 0  # Start in initial state
        stack = []  # Used for backtracking when we hit errors
        lexeme = ""  # Will store the text of the token we're building
        
        # Push error marker onto stack
        stack.append(-2)  
        
        # Process characters until we hit an error state (-1)
        while state != -1:
            # If current state is accepting, clear the stack
            # (we only want to keep track back to last accepting state)
            if self.AcceptingStates(state): 
                stack.clear()
            
            # Remember this state in case we need to backtrack
            stack.append(state)
            
            # Get next character
            exists, character = self.NextChar(src_program_str, src_program_idx)
            lexeme += character
            

            # Stop if we're at end of input
            if not exists: 
                break
            
            

            # Move to next character position
            src_program_idx += 1
            

            # Categorize the character and look up next state
            cat = self.CatChar(character)
            state = self.Tx[state][self.lexeme_list.index(cat)]
            

            

            # Debug print (commented out)
            #print("Lexeme: ", lexeme, " => NEXT STATE: ", state, "  => CAT: ", cat, "  => CHAR:", character, "  => STACK: ", stack)
        
        # Remove the last character that caused the error state
        lexeme = lexeme[:-1]

        syntax_error = False
        
        # Now backtrack to find the last accepting state
        while len(stack) > 0:
            if stack[-1] == -2:  # Hit our error marker
                syntax_error = True
                # Get the problematic character
                exists, character = self.NextChar(src_program_str, src_program_idx-1)
                lexeme = character
                break    
            
            # If top of stack isn't accepting, backtrack
            if not self.AcceptingStates(stack[-1]):
                stack.pop()
                lexeme = lexeme[:-1]  # Remove last character from lexeme
            else:
                # Found an accepting state - use this
                state = stack.pop()
                break
        

        # Handle syntax errors
        if syntax_error:
            return Token(TokenType.error, lexeme), "error"
        
        # Return appropriate token if in accepting state
        if self.AcceptingStates(state):
            return self.GetTokenTypeByFinalState(state, lexeme), lexeme
        else: 
            return Token(TokenType.error, lexeme), "Lexical Error"

        

    # Main function to tokenize entire input string
    def GenerateTokens(self, src_program_str):
        print("INPUT:: " + src_program_str)
        tokens_list = []
        src_program_idx = 0
        
        # Get first token
        token, lexeme = self.NextToken(src_program_str, src_program_idx)
        tokens_list.append(token)

        # Continue getting tokens until end of input or error
        while token != -1:
            # Move index past current token
            src_program_idx += len(lexeme)
            
            # If not at end, get next token
            if not self.EndOfInput(src_program_str, src_program_idx):
                token, lexeme = self.NextToken(src_program_str, src_program_idx)
                tokens_list.append(token)
                if token.type == TokenType.error: 
                    break  # Stop on error
            else: 
                break  # Stop at end of input

        return tokens_list

# # Test the lexer
lex = Lexer()
toks = lex.GenerateTokens(""" 
            x = bozo(5);
                
                """)

 #Print all found tokens
for t in toks:
    print(t.type, t.lexeme)