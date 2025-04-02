# Import Enum class from enum module to create enumerated constants
from enum import Enum

# Define different types of tokens our lexer will recognize
class TokenType(Enum):
    keyword = 1
    identifier = 2    # For variable names, function names, etc.
    whitespace = 3    # For spaces, tabs, etc.
    iteral = 4        # For values
    seperator = 5     # For seperators/punctuator
    operator = 6
    error = 7        # For invalid tokens
    end = 8           # For end of input

# Class to represent a token with its type and actual text (lexeme)
class Token:
    def __init__(self, t, l):
        self.type = t  # Token type (from TokenType enum)
        self.lexeme = l  # The actual text of the token

# Main lexer class that converts source code into tokens
class Lexer:
    def __init__(self):
        # Categories of characters we'll encounter
        self.lexeme_list = ["_", ".", "\"", "letter", "digit","seperator","operator","boolean_value", "ws", "other"]

        self.keyword_list = {"for","let","while","return", "fun", "int", "float" ,"bool","colour","as",}
        
        self.boolean_list = {"true", "false"}
        
        # Possible states of our finite automaton
        self.states_list = [0, 1, 2, 3, 4, 5, 6, 7,8,9]
        
        # Which states are accepting (valid end states for tokens)
        self.states_accp = [1, 2, 3, 4, 5, 7,9]

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
        self.Tx[0][self.lexeme_list.index("seperator")] = 4

        self.Tx[0][self.lexeme_list.index("operator")] = 5

        self.Tx[0][self.lexeme_list.index("\"")] = 8
        self.Tx[8][self.lexeme_list.index("digit")] = 8
        self.Tx[8][self.lexeme_list.index("letter")] = 8
        self.Tx[8][self.lexeme_list.index("\"")] = 9

        # Print the transition table for debugging
        for row in self.Tx:
            print(row)

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
            if lexeme in self.keyword_list:
                return Token(TokenType.keyword, lexeme)
            elif lexeme in self.boolean_list: 
                return Token(TokenType.iteral, lexeme)
            else:
                return Token(TokenType.identifier, lexeme)
        elif state == 2:  # Whitespace state
            return Token(TokenType.whitespace, lexeme)
        elif state == 3:
            return Token(TokenType.iteral, lexeme )
        elif state == 4:
            return Token(TokenType.seperator, lexeme)
        elif state == 5:
            return Token(TokenType.operator, lexeme)
        elif state == 7: 
            return Token(TokenType.iteral, lexeme)
        elif state == 9:
            return Token(TokenType.iteral, lexeme)
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
        if character == " ": cat = "ws"      
        if character in {"(",")","{","}",";",":",","}: cat = "seperator"
        if character in {"+","-","<",">","=","->","/","and","or"}: cat = "operator"
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

# Test the lexer
lex = Lexer()
toks = lex.GenerateTokens("fun XGreaterY_2(x:int, y:int) -> \"bozo\" true 3.543 {")

# Print all found tokens
for t in toks:
    print(t.type, t.lexeme)