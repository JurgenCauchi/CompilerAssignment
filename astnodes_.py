# First some AST Node classes we'll use to build the AST with

# Base class for all AST nodes
class ASTNode:
    def __init__(self):
        self.name = "ASTNode"  # Identifier for the node type

# Base class for all statement nodes (extends ASTNode)
class ASTStatementNode(ASTNode):
    def __init__(self):
        super().__init__()
        self.name = "ASTStatementNode"

# Base class for all expression nodes (extends ASTNode)
class ASTExpressionNode(ASTNode):
    def __init__(self):
        super().__init__()
        self.name = "ASTExpressionNode"

# Node for variables (extends expression node since variables can be used in expressions)
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

class ASTColonNode(ASTExpressionNode):
    def __init__(self, lexeme):
        super().__init__()
        self.name = "ASTColonNode"
        self.lexeme = lexeme  # The actual variable name (e.g., "x", "count")

    # Visitor pattern accept method
    def accept(self, visitor):
        visitor.visit_colon_node(self)


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

# Node for assignment statements (extends statement node)
class ASTAssignmentNode(ASTStatementNode):
    def __init__(self, ast_type_node, ast_colon_node, ast_var_node, ast_expression_node):
        super().__init__()
        self.name = "ASTStatementNode"  
        self.type = ast_type_node      # Stores the type of the variable
        self.colon = ast_colon_node
        self.id = ast_var_node         # Left-hand side (variable being assigned to)
        self.expr = ast_expression_node # Right-hand side (expression being assigned)

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

    def visit_type_node(self, node):
        raise NotImplementedError()

    def visit_colon_node(self,node):
        raise NotADirectoryError()
    
    def visit_integer_node(self, node):
        raise NotImplementedError()
    
    def visit_float_node(self, node):
        raise NotImplementedError()

    def visit_colour_node(self, node):
        raise NotImplementedError()
    
    def visit_boolean_node(self, node):
        raise NotImplementedError()

    def visit_assignment_node(self, node):
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
    def visit_assignment_node(self, ass_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Assignment node => ")
        self.inc_tab_count()        
        ass_node.type.accept(self)
        ass_node.colon.accept(self)
        ass_node.id.accept(self)    # Visit the left-hand side
        ass_node.expr.accept(self)   # Visit the right-hand side
        self.dec_tab_count()

    # Visit a variable node
    def visit_variable_node(self, var_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Variable => ", var_node.lexeme)

    def visit_type_node(self, type_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Type => ", type_node.lexeme)

    def visit_colon_node(self, colon_node):
        self.node_count += 1
        print('\t' * self.tab_count, "Colon => ", colon_node.lexeme)

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
assignment_type = ASTTypeNode("int")
assignment_colon = ASTColonNode(":")
assignment_lhs = ASTVariableNode("x")  # Left side is variable 'x'
assignment_rhs = ASTIntegerNode(23)    # Right side is integer 23
root = ASTAssignmentNode(assignment_type, assignment_colon, assignment_lhs, assignment_rhs)  # Combine into assignment

# Print the AST structure
root.accept(print_visitor)
print("Node Count => ", print_visitor.node_count)
print("----")

