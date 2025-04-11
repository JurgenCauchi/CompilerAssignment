import parserblock as par
class Visitor:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'  # e.g., "visit_ASTForNode"
        visitor_method = getattr(self, method_name, None)
        
        if visitor_method is not None:
            return visitor_method(node)
        else:
            return None   

class TypeChecker(Visitor):
    def __init__(self):
        self.scopes = [{}]  # Tracks variable types
        self.errors = []  # Collect errors instead of raising immediately

    def current_scope(self):
        return self.scopes[-1]

    def enter_scope(self):
        self.scopes.append({})

    def exit_scope(self):
        self.scopes.pop()

    def declare_variable(self, name, var_type):
        if name in self.current_scope():
            self._add_error(f"Variable '{name}' already declared in this scope")
        else:
            self.current_scope()[name] = var_type

    def lookup_variable(self, name):
        for scope in reversed(self.scopes):
            if name in scope:
                return scope[name]
        return None

    def visit_ASTProgramNode(self, node):
        print(f"Checking program with {len(node.statements)} statements")
        for stmt in node.statements:
            self.visit(stmt)
        
        if self.errors:
            print("\nType errors found:")
            for error in self.errors:
                print(f"- {error}")
        
    def visit_ASTRelOpNode(self, node):

        left_type = self.visit(node.left)
        right_type = self.visit(node.right)

        # Skip if either operand has an error (e.g., undeclared var)
        if "error" in (left_type, right_type):
            return "error"

        return "bool"  # Default return type for relational ops

    def visit_ASTDeclarationNode(self, node):
            
            var_name = node.id.lexeme
            var_type = node.type.lexeme

            # Check initialization expression type
            if node.expr:
                expr_type = self.visit(node.expr)
                if expr_type != var_type:
                    self._add_error(f"Type mismatch: cannot assign {expr_type} to {var_type} variable '{var_name}'")

            self.declare_variable(var_name, var_type)
            
    def visit_ASTAssignmentNode(self, node):

        var_name = node.id.lexeme
        var_type = self.lookup_variable(var_name)

        if var_type is None:
            self._add_error(f"Variable '{var_name}' not declared")
            return

        expr_type = self.visit(node.expr)

        if expr_type != var_type:
            self._add_error(f"Type mismatch: cannot assign {expr_type} to {var_type} variable '{var_name}'")

        return var_type

    def visit_ASTForNode(self, node):
        """Type-check a for loop: for(init; condition; update) { body }"""
        # 1. Check initializer (e.g., 'let int i = 0')
        self.enter_scope()

        if node.vardec:
            self.visit(node.vardec)

        # 2. Check condition (e.g., 'x < 10')
        if node.expr:
            self.visit(node.expr)  # This will trigger visit_ASTRelOpNode

        # 3. Check update (e.g., 'x = i + 1')
        if node.assgn:
            self.visit(node.assgn)  # This will detect undeclared 'x'

        # 4. Check body (e.g., '{ let int n = 6 }')
        if node.blck:
            self.enter_scope()  # New scope for loop variables (inside the body)
            self.visit(node.blck)
            self.exit_scope()  # Exit after the body

        self.exit_scope()  # Exit the outer scope after the loop ends

    def visit_ASTWhileNode(self, node):
        """Type-check a for loop: for(init; condition; update) { body }"""
        
        self.enter_scope()

        # 1. Check condition (e.g., 'x < 10')
        if node.expr:
            self.visit(node.expr)  # This will trigger visit_ASTRelOpNode

        # 2. Check body (e.g., '{ let int n = 6 }')
        if node.block:
            self.enter_scope()  # New scope for loop variables (inside the body)
            self.visit(node.block)
            self.exit_scope()  # Exit after the body

        self.exit_scope()  # Exit the outer scope after the loop ends

    def visit_ASTIfNode(self, node):
        """Type-check a for loop: for(init; condition; update) { body }"""

        self.enter_scope()

        # 1. Check condition (e.g., 'x < 10')
        if node.expr:
            self.visit(node.expr)  # This will trigger visit_ASTRelOpNode

        # 2. Check body (e.g., '{ let int n = 6 }')
        if node.blocks:
            self.enter_scope()  # New scope for loop variables (inside the body)
            self.visit(node.blocks)
            self.exit_scope()  # Exit after the body

        self.exit_scope()  # Exit the outer scope after the loop ends

    def visit_ASTBlockNode(self, node):
        self.enter_scope()
        for stmt in node.stmts:
            self.visit(stmt)
        self.exit_scope()

    def visit_ASTProgramNode(self, node):
        """Handle the root program node"""
        print(f"Type checking program with {len(node.statements)} statements")  # Debug
        for statement in node.statements:
            self.visit(statement)

    def visit_ASTAddOpNode(self, node):
        
        # Visit both operands first

        left_type = self.visit(node.left)
        right_type = self.visit(node.right)
        
        # Handle error cases
        if left_type == "error" or right_type == "error":
            return "error"
        if left_type is None:
            self._add_error(f"Left operand has no type (missing visitor for {type(node.left).__name__}?)")
            return "error"
        if right_type is None:
            self._add_error(f"Right operand has no type (missing visitor for {type(node.right).__name__}?)")
            return "error"
        
        # Type checking
        if left_type != right_type:
            self._add_error(f"Type mismatch in operation: {left_type} {node.op} {right_type}")
            return "error"
        
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
        """Handle relational operations like <, >, =="""
        # First check if either operand is an undeclared variable
        left_type = self.visit(node.left)
        right_type = self.visit(node.right)

        # Skip further checks if either operand had an error
        if "error" in (left_type, right_type):
            return "error"

        return "bool"  # Relational ops always return bool
    
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
    
    def visit_ASTVariableNode(self, node):
        """Handle variable references like 'x' in expressions"""
        # For consistency, make this identical to your ASTIVariableNode visitor
        var_type = self.lookup_variable(node.lexeme)
        if var_type is None:
            self._add_error(f"Undeclared variable: '{node.lexeme}'")
            return "error"
        return var_type
    
    def visit_ASTExpressionNode(self, node):
        raise SyntaxError("ooga booga")


    def _add_error(self, message):
        """Helper to collect errors"""
        self.errors.append(message)


# Assuming you have an AST root node from your parser

parser = par.Parser(""" 

            { let int:x = 0;
           while(x==5){
                    }}

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