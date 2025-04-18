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
            self.func_sigs = {}
            self.current_return_type = None 

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

        def can_cast(self, from_type, to_type):
            # Identity cast (always allowed)
            if from_type == to_type:
                return True

            # Define allowed casts
            allowed_casts = {
                'int': 'float',
                'float': 'int',
                'bool': 'int',
                'colour': 'int',
                'int': 'colour',
            }

            # Check if casting is allowed
            return to_type in allowed_casts.get(from_type, set())

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
        
        def visit_ASTCastNode(self, node):
            expr_type = self.visit(node.expr)
            target_type = node.to_type
            if not self.can_cast(expr_type, target_type):
                raise TypeError(f"Cannot cast from {expr_type} to {target_type}")
            return target_type
        
        def visit_ASTReturnNode(self, node):
            if self.current_return_type is None:
                self._add_error("Return statement outside of any function")
                return

            if node.expr:
                actual = self.visit(node.expr)
                expected = self.current_return_type
                if actual != expected:
                    self._add_error(
                        f"Return type mismatch: function expects '{expected}', but returning '{actual}'"
                    )
            else:
                # a bare `return;` in a non-void function
                if self.current_return_type != "void":
                    self._add_error(
                        f"Return statement missing expression (expected '{self.current_return_type}')"
                    )

        def visit_ASTFunctionDeclNode(self, node):
            # 1) Extract name and types using .lexeme
            name     = node.identifier  
            param_ts = [p[1] for p in node.formalparams]  
            ret_t    = node.type             

            # 2) Register signature
            self.func_sigs[name] = (param_ts, ret_t)

            # 3) Type‑check body with current_return_type set
            old_ret = self.current_return_type
            self.current_return_type = ret_t

            self.enter_scope()
            # declare each parameter in the new scope
            for idTok, typeTok in node.formalparams:
                self.declare_variable(idTok, typeTok)

            # visit every statement in the function block
            for stmt in node.block.stmts:
                self.visit(stmt)

            self.exit_scope()
            self.current_return_type = old_ret

        def visit_ASTFunctionCallNode(self, node):
            name = node.ident              # your parser probably sets ident to the lexeme
            sig  = self.func_sigs.get(name)
            if not sig:
                self._add_error(f"Call to undefined function '{name}'")
                return None

            param_ts, ret_t = sig
            if len(node.params) != len(param_ts):
                self._add_error(
                    f"Function '{name}' expects {len(param_ts)} args, got {len(node.params)}"
                )

            for arg, expected in zip(node.params, param_ts):
                actual = self.visit(arg)
                if actual != expected:
                    self._add_error(
                        f"Type mismatch in call to '{name}': expected {expected}, got {actual}"
                    )

            return ret_t
        
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
            # 2. Visit the 'then' block (if it exists)
            if node.blocks and len(node.blocks) > 0:
                self.enter_scope()  # New scope for the 'then' block
                then_block = node.blocks[0]
                for stmt in then_block.stmts:  # Iterate through statements in the block
                    self.visit(stmt)  # This will visit any ASTReturnNode inside
                self.exit_scope()

            # 3. Visit the 'else' block (if it exists)
            if node.blocks and len(node.blocks) > 1:
                self.enter_scope()  # New scope for the 'else' block
                else_block = node.blocks[1]
                for stmt in else_block.stmts:  # Iterate through statements in the block
                    self.visit(stmt)  # This will visit any ASTReturnNode inside
                self.exit_scope()
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

        def visit_ASTPadRandINode(self,node):
            expr = self.visit(node.expr)
            return expr
        
        def visit_ASTPrintNode(self,node):
            expr = self.visit(node.expr)
            return expr

        def visit_ASTDelayNode(self,node):
            expr = self.visit(node.expr)
            return expr

        def visit_ASTWriteNode(self, node):
            # 1. Check argument count
            if node.kw == 0 and node.size != 5:
                self._add_error("Write Box must have exactly 5 arguments (x, y, width, height, colour)")
            elif node.kw == 1 and node.size != 3:
                self._add_error("Write must have exactly 3 arguments (x, y, colour)")

            # 2. Check types of each argument
            expected_types = []
            if node.kw == 0:  # wrbox: (int, int, int, int, colour)
                expected_types = ["int", "int", "int", "int", "colour"]
                arg_names = ["x-coord", "y-coord", "width", "height", "colour"]
            elif node.kw == 1:  # write: (int, int, colour)
                expected_types = ["int", "int", "colour"]
                arg_names = ["x-coord", "y-coord", "colour"]

            # Ensure we have enough expected types (avoid index errors)
            if len(node.expressions) != len(expected_types):
                return  # Already handled by size check above
            
            for i in range(len(node.expressions)):
                expr = node.expressions[i]
                expected_type = expected_types[i]
                actual_type = self.visit(expr)

                if actual_type != expected_type:
                    self._add_error(
                        f"Invalid type for {arg_names[i]} in {'wrbox' if node.kw == 0 else 'write'}: "
                        f"Expected {expected_type}, got {actual_type}"
                    )

        def visit_ASTPadHeightNode(self,node):
            return "int"
        
        def visit_ASTPadWidthNode(self,node):
            return "int"

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

# parser = par.Parser(""" 

#                   fun giga()-> int {
#                     return 5;
#                     }
#                 let x:int = giga();

#                     """)

# # ast_root = parser.Parse()

#     # Create and run the type checker
# type_checker = TypeChecker()
# type_checker.visit(ast_root)

# if type_checker.errors:
#     print("Type checking failed with the following errors:")
#     for error in type_checker.errors:
#         print(f"- {error}")
# else:
#     print("Type checking passed!")