import parserblock as par

class Visitor:
    def visit(self, node):
        """Dispatch to the appropriate visit method based on node type"""
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)
    
    def generic_visit(self, node):
        """Called if no specific visit method exists for the node type"""
        raise NotImplementedError(f'No visit method for {type(node).__name__}')
    

class TypeChecker(Visitor):
    def __init__(self):
        self.symbol_table = {}  # Tracks variable types
        self.current_scope = "global"  # For scope management
        self.errors = []  # Collect errors instead of raising immediately

    def visit_ASTProgramNode(self, node):
        print(f"Checking program with {len(node.statements)} statements")
        for stmt in node.statements:
            self.visit(stmt)
        
        if self.errors:
            print("\nType errors found:")
            for error in self.errors:
                print(f"- {error}")
    
    def visit_ASTDeclarationNode(self, node):
        """Handle variable declarations matching your actual node structure"""
        # Get variable info from the node structure
        var_name = node.id.lexeme  # Assuming ast_var_node has a 'value' attribute
        var_type = node.type.lexeme  # Assuming ast_type_node has a 'value'
        
        # Check if variable already exists
        if var_name in self.symbol_table:
            self._add_error(f"Variable '{var_name}' already declared")
            return
        
        # Check initialization expression if it exists
        if node.expr:  # Using node.expr as per your structure
            expr_type = self.visit(node.expr)
            if expr_type != var_type:
                self._add_error(f"Type mismatch: cannot assign {expr_type} to {var_type} variable '{var_name}'")
        
        # Add to symbol table
        self.symbol_table[var_name] = var_type


    
    def visit_ASTProgramNode(self, node):
        """Handle the root program node"""
        print(f"Type checking program with {len(node.statements)} statements")  # Debug
        for statement in node.statements:
            self.visit(statement)

    def visit_ASTAddOpNode(self, node):
        """Handle binary operations like +, -, *, /"""
        left_type = self.visit(node.left)
        right_type = self.visit(node.right)
        
        # Type checking rules
        if left_type != right_type:
            self._add_error(f"Type mismatch in operation: {left_type} {node.op} {right_type}")
            return "error"
        
        # Return the result type (could be more sophisticated)
        return left_type

    def visit_ASTMultiOpNode(self, node):
        """Handle binary operations like +, -, *, /"""
        left_type = self.visit(node.left)
        right_type = self.visit(node.right)
        
        # Type checking rules
        if left_type != right_type:
            self._add_error(f"Type mismatch in operation: {left_type} {node.op} {right_type}")
            return "error"
        
        # Return the result type (could be more sophisticated)
        return left_type


    def visit_ASTRelOpNode(self, node):
        """Handle binary operations like +, -, *, /"""
        left_type = self.visit(node.left)
        right_type = self.visit(node.right)
        
        # Type checking rules
        if left_type != right_type:
            self._add_error(f"Type mismatch in operation: {left_type} {node.op} {right_type}")
            return "error"
        
        # Return the result type (could be more sophisticated)
        return left_type
    
    
    def visit_ASTIntegerNode(self, node):
        """Handle integer literals"""
        return "int"  # All integer literals have type 'int'
    
    def visit_ASTFloatNode(self, node):
        """Handle float literals"""
        return "float"
    
    def visit_ASTBooleanNode(self, node):
        """Handle boolean literals"""
        return "bool"
    
    def visit_ASTColourNode(self, node):
        """Handle colour literals"""
        return "colour"
    
    def visit_ASTIVariableNode(self, node):
        """Handle variable references"""
        if node.name not in self.symbol_table:
            self._add_error(f"Undeclared variable: {node.name}")
            return "error"
        return self.symbol_table[node.name]
    
    def visit_ASTExpressionNode(self, node):
        raise TypeError(f"Cannot type check generic ASTExpressionNode directly. Node: {node}")


    def _add_error(self, message):
        """Helper to collect errors"""
        self.errors.append(message)


# Assuming you have an AST root node from your parser

parser = par.Parser(""" 
                
                x= __padread 5,5;


                """)

ast_root = parser.Parse()

# Create and run the type checker
type_checker = TypeChecker()
type_checker.visit(ast_root)

if type_checker.errors:
    print("Type checking failed with the following errors:")
    for error in type_checker.errors:
        print(f"- {error}")
else:
    print("Type checking passed!")