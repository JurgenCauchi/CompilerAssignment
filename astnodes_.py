# First some AST Node classes we'll use to build the AST with

# Base class for all AST nodes
class ASTNode:
    def __init__(self):
        self.name = "ASTNode"  # Identifier for the node type

# Node for the entire program (extends ASTNode)
class ASTProgramNode(ASTNode):
    def __init__(self):
        super().__init__()
        self.name = "ASTProgramNode"
        self.statements = []  # List of all top-level statements in the program

    # Add a statement to the program
    def add_statement(self, statement):
        self.statements.append(statement)

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_program_node(self)


# Base class for all statement nodes (extends ASTNode)
class ASTStatementNode(ASTNode):
    def __init__(self ):
        super().__init__()
        self.name = "ASTStatementNode"

    def accept(self, visitor):
        visitor.visit_statement_node(self)


class ASTUnaryNode(ASTNode):
    def __init__(self, op, expr):
        self.op = op  # '-' or 'not'
        self.expr = expr          # an instance of Expr (AST node)
    

# Base class for all expression nodes (extends ASTNode)
class ASTExpressionNode(ASTNode):
    def __init__(self, op=None, left=None, right=None, value=None, cast_type=None):
        super().__init__()
        self.name = "ASTExpressionNode"

        # For binary/unary operations
        self.op = op        # E.g. '+', '-', '*', '/', '<', '>', etc.
        self.left = left    # ASTExpressionNode or ASTFactorNode
        self.right = right  # ASTExpressionNode or ASTFactorNode

        # For leaf nodes like literals, identifiers, function calls
        self.value = value  # E.g. 2, "x", FunctionCallNode, PadReadNode, etc.

        # For type casting
        self.cast_type = cast_type  # E.g. "int", "float", etc.

    def __str__(self):
        if self.value is not None:
            return f"Value({self.value})"
        elif self.op is not None:
            return f"({self.left} {self.op} {self.right})"
        return "Expression"
    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_expression_node(self)

# Node for variables (extends expression node since variables can be used in expressions)
class ASTFunctionDeclNode(ASTExpressionNode):
    def __init__(self, ident,formalpar,type,intlit,block):
        super().__init__()
        self.name = "ASTFunctionDeclNode"
        self.identifier = ident  
        self.formalparams = formalpar
        self.type = type
        self.intlit = intlit
        self.block = block

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_functiondecl_node(self)


class ASTVariableNode(ASTExpressionNode):
    def __init__(self, lexeme):
        super().__init__()
        self.name = "ASTVariableNode"
        self.lexeme = lexeme  # The actual variable name (e.g., "x", "count")

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_variable_node(self)


class ASTTypeNode(ASTExpressionNode):
    def __init__(self, lexeme):
        super().__init__()
        self.name = "ASTTypeNode"
        self.lexeme = lexeme  # The actual variable name (e.g., bool, int)

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_type_node(self)


# Node for integer literals (extends expression node)
class ASTIntegerNode(ASTExpressionNode):
    def __init__(self, v):
        super().__init__()
        self.name = "ASTIntegerNode"
        self.value = v  # The integer value (e.g., 42, 100)

    
    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_integer_node(self)        

class ASTFloatNode(ASTExpressionNode):
    def __init__(self, v):
        super().__init__()
        self.name = "ASTFloatNode"
        self.value = v  # The integer value (e.g., 4.3, 66,84)

    
    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_float_node(self)     

#node for colour literals
class ASTColourNode(ASTExpressionNode):
    def __init__(self, v):
        super().__init__()
        self.name = "ASTColourNode"
        self.value = v  # The Colour value (e.g., #672382, #bd45c1)

    
    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_colour_node(self)    

class ASTBooleanNode(ASTExpressionNode):
    def __init__(self, v):
        super().__init__()
        self.name = "ASTBooleanNode"
        self.value = v  # The boolean value (e.g., true, false)

    
    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_boolean_node(self)    


class ASTMultiOpNode(ASTExpressionNode):
    def __init__(self, op, left, right):
        super().__init__()
        self.name = "ASTBinaryOpNode"
        self.op = op          # The operator (e.g., "*", "/", "and")
        self.left = left      # Left operand (ASTExpressionNode)
        self.right = right    # Right operand (ASTExpressionNode)

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_multi_op_node(self)


# Node for assignment statements (extends statement node)
class ASTDeclarationNode(ASTStatementNode):
    def __init__(self, ast_type_node,  ast_var_node, ast_expression_node):
        super().__init__()
        self.name = "ASTDeclarationNode"
        self.type = ast_type_node      # Stores the type of the variable
        self.id = ast_var_node         # Left-hand side (variable being assigned to)
        self.expr = ast_expression_node # Right-hand side (expression being assigned)

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_declaration_node(self)                

class ASTReturnNode(ASTStatementNode):
    def __init__(self, ast_return_node):
        super().__init__()
        self.name = "ASTReturnNode"
        self.expr = ast_return_node      # Stores the type of the variabl

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_return_node(self)                

class ASTPrintNode(ASTStatementNode):
    def __init__(self, ast_print_node):
        super().__init__()
        self.name = "ASTPrintNode"
        self.expr = ast_print_node      # Stores the type of the variabl

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_print_node(self)    

class ASTForNode(ASTStatementNode):
    def __init__(self, ast_exp_node, ast_vardec_node, ast_ass_node, ast_blck_node):
        super().__init__()
        self.name = "ASTForNode"
        self.vardeclr = ast_vardec_node    #stores the variable declaration 
        self.expr = ast_exp_node      # Stores the expression
        self.assgn = ast_ass_node
        self.blck = ast_blck_node


    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_for_node(self)    


class ASTIfNode(ASTStatementNode):
    def __init__(self, ast_exp_node, ast_blocks,):
        super().__init__()
        self.name = "ASTIfNode"  
        self.expr = ast_exp_node      # Stores the expression
        self.blocks = ast_blocks



    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_if_node(self)    

class ASTWhileNode(ASTStatementNode):
    def __init__(self, ast_exp_node, ast_block):
        super().__init__()
        self.name = "ASTWhileNode"  
        self.expr = ast_exp_node      # Stores the expression
        self.block = ast_block



    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_while_node(self)    



class ASTDelayNode(ASTStatementNode):
    def __init__(self, ast_delay_node):
        super().__init__()
        self.name = "ASTdelayNode"
        self.expr = ast_delay_node      # Stores the type of the variabl

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_delay_node(self)


class ASTWriteNode(ASTStatementNode):
    def __init__(self, expressions):  # Now takes a list
        super().__init__()
        self.name = "ASTWriteNode"
        self.expressions = expressions  # List of AST nodes

    def accept(self, visitor):
        visitor.visit_write_node(self)    


class ASTAssignmentNode(ASTStatementNode):
    def __init__(self, ast_id_node, ast_assignment_node):
        super().__init__()
        self.name = "ASTAssignmentNode"
        self.id = ast_id_node
        self.expr = ast_assignment_node      # Stores the type of the variabl

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_assignment_node(self)                


# Node for blocks of statements (like { stmt1; stmt2; })
class ASTBlockNode(ASTNode):
    def __init__(self):
        super().__init__()
        self.name = "ASTBlockNode"
        self.stmts = []  # List of statements in this block

    # Add a statement to this block
    def add_statement(self, node):
        self.stmts.append(node)

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_block_node(self)        

# Abstract base class for AST visitors (implements Visitor pattern)
class ASTVisitor:
    # These methods must be implemented by concrete visitors
    def visit_program_node(self, node):
        raise NotImplementedError()
    
    def visit_multi_op_node(self, node):
        raise NotImplementedError()
    
    def visit_type_node(self, node):
        raise NotImplementedError()
    
    def visit_integer_node(self, node):
        raise NotImplementedError()
    
    def visit_float_node(self, node):
        raise NotImplementedError()

    def visit_colour_node(self, node):
        raise NotImplementedError()
    
    def visit_boolean_node(self, node):
        raise NotImplementedError()

    def visit_decleration_node(self, node):
        raise NotImplementedError()
    
    def visit_variable_node(self, node):
        raise NotImplementedError()
    
    def visit_block_node(self, node):
        raise NotImplementedError()
    
    # Methods for managing indentation level (for pretty printing)
    def inc_tab_count(self):
        raise NotImplementedError()
    
    def dec_tab_count(self):
        raise NotImplementedError()

# Concrete visitor that prints the AST structure
class PrintNodesVisitor(ASTVisitor):

    def visit_program_node(self, program_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Program:")
        self.inc_tab_count()
        
        # Visit each statement in the program
        for stmt in program_node.statements:
            stmt.accept(self)
        
        self.dec_tab_count()

    def __init__(self):
        self.name = "Print Tree Visitor"
        self.node_count = 0  # Counts how many nodes we visit
        self.tab_count = 0   # Tracks indentation level for printing

    # Increase indentation level
    def inc_tab_count(self):
        self.tab_count += 1

    # Decrease indentation level
    def dec_tab_count(self):
        self.tab_count -= 1

    def visit_return_node(self, return_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Return Statement:")
        self.inc_tab_count()
        
        if return_node.expr:  # Print return value if exists
            return_node.expr.accept(self)
        else:
            print('\t' * self.tab_count, "(void)")
            
        self.dec_tab_count()


    def visit_functiondecl_node(self, function_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Function Declaration Statement:")
        self.inc_tab_count()
        
        # Handle strings directly
        print('\t' * self.tab_count, "Identifier:", function_node.identifier)
        print('\t' * self.tab_count, "Return Type:", function_node.type)

        if function_node.intlit:
            print('\t' * self.tab_count, "Array Size:", function_node.intlit)

        if function_node.formalparams:
            print('\t' * self.tab_count, "Parameters:")
            self.inc_tab_count()
            for param in function_node.formalparams:
                print('\t' * self.tab_count, param)  # Or param.accept() if params are nodes
            self.dec_tab_count()
        else:
            print('\t' * self.tab_count, "(void)")
            
        function_node.block.accept(self)  # block should be an AST node
        
        self.dec_tab_count()

    def visit_for_node(self, for_node):
        self.node_count += 1
        print('\t' * self.tab_count, "For loop Statement node => ")
        self.inc_tab_count()    
        for_node.expr.accept(self) 
        for_node.vardeclr.accept(self) 
        for_node.assgn.accept(self)
        for_node.blck.accept(self)
        self.dec_tab_count()
            
    def visit_if_node(self, if_node):
        self.node_count += 1
        print('\t' * self.tab_count, "If Statement node => ")
        self.inc_tab_count()    
        if_node.expr.accept(self) 
        for block in if_node.blocks:
            block.accept(self)
        self.dec_tab_count()

    def visit_while_node(self, if_node):
        self.node_count += 1
        print('\t' * self.tab_count, "While Statement node => ")
        self.inc_tab_count()    
        if_node.expr.accept(self) 
        if_node.block.accept(self)
        self.dec_tab_count()

    # Visit an integer node
    def visit_integer_node(self, int_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Integer value::", int_node.value)

    def visit_float_node(self, float_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Float value::", float_node.value)

    def visit_colour_node(self, colour_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Colour value::", colour_node.value)

    def visit_boolean_node(self, boolean_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Boolean value::", boolean_node.value)

    # Visit an assignment node
    def visit_declaration_node(self, ass_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Declaration node => ")
        self.inc_tab_count()    
        ass_node.id.accept(self)    # Visit the left-hand side
        ass_node.expr.accept(self)   # Visit the right-hand side
        self.dec_tab_count()

    def visit_assignment_node(self, ass_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Assignment node => ")
        self.inc_tab_count()   
        ass_node.id.accept(self)    # Visit the left-hand side
        ass_node.expr.accept(self)   # Visit the right-hand side
        self.dec_tab_count()

    def visit_print_node(self, print_node):
        self.node_count += 1
        print('\t' * self.tab_count, "print node => ")
        self.inc_tab_count()  
        print_node.expr.accept(self)   # Visit the expression
        self.dec_tab_count()

    def visit_delay_node(self, delay_node):
        self.node_count += 1
        print('\t' * self.tab_count, "delay node => ")
        self.inc_tab_count()  
        delay_node.expr.accept(self)   # Visit the expression
        self.dec_tab_count()

    def visit_write_node(self, write_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Write node =>")
        self.inc_tab_count()
        
        for expr in write_node.expressions:
            expr.accept(self)
        
        self.dec_tab_count()

    # Visit a variable node
    # Visit a variable node
    def visit_variable_node(self, var_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Variable => ", var_node.lexeme)

    def visit_expression_node(self, var_node):
        self.node_count += 1
        print('\t' * self.tab_count, f"Expression => Operator: {var_node.op}")
        
        self.tab_count += 1
        
        # Left side
        if hasattr(var_node, 'left') and var_node.left:
            print('\t' * self.tab_count, "Left:")
            self.tab_count += 1
            var_node.left.accept(self)
            self.tab_count -= 1

        # Right side
        if hasattr(var_node, 'right') and var_node.right:
            print('\t' * self.tab_count, "Right:")
            self.tab_count += 1
            var_node.right.accept(self)
            self.tab_count -= 1

        # Optional cast
        if hasattr(var_node, 'cast_type') and var_node.cast_type:
            print('\t' * self.tab_count, f"Cast to: {var_node.cast_type}")

        self.tab_count -= 1


    def visit_type_node(self, type_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Type => ", type_node.lexeme)

    # Visit a block node
    def visit_block_node(self, block_node):
        self.node_count += 1
        print('\t' * self.tab_count, "New Block => ")
        self.inc_tab_count()
        
        # Visit each statement in the block
        for st in block_node.stmts:
            st.accept(self)
        
        self.dec_tab_count()

# Example 1: Building and printing AST for "x = 23;"
print("Building AST for assigment statement x=23;")

# Create a print visitor instance
print_visitor = PrintNodesVisitor()

# Create nodes for the assignment:
#assignment_type = ASTTypeNode("int")
#assignment_colon = ASTColonNode(":")
#ssignment_lhs = ASTVariableNode("x")  # Left side is variable 'x'
#assignment_rhs = ASTIntegerNode(23)    # Right side is integer 23
#root = ASTAssignmentNode(assignment_type, assignment_colon, assignment_lhs, assignment_rhs)  # Combine into assignment

# Print the AST structure
#root.accept(print_visitor)
print("Node Count => ", print_visitor.node_count)
print("----")

