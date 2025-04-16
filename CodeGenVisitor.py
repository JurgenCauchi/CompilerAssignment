import parserblock as par
from SemanticVisitor import Visitor
from SemanticVisitor import TypeChecker
import pyperclip



class CodeGenVisitor(Visitor):
    def __init__(self):
        self.instructions = []
        self.scopes = [{}]  # memory stack (SoF), stores (level, index) for each variable
        self.level = 0    # level in the SoF (stack of frames)

    def visit(self, node):
        """Dispatches the visit method based on the node type."""
        method_name = f"visit_{type(node).__name__}"
        visit_method = getattr(self, method_name, self.generic_visit)
        return visit_method(node)

    def generic_visit(self, node):
        """Handle generic cases or nodes with no specific visit method."""
        print(f"Visiting unhandled node type: {type(node).__name__}")

    def emit(self, line):
        self.instructions.append(line)

    def enter_scope(self):
        self.scopes.append({})
        self.level += 1

    def exit_scope(self):
        self.scopes.pop()
        self.level -= 1

    def declare_variable(self, name):
        if not self.scopes:
            raise Exception("No scope available to declare a variable.")
        index = len(self.scopes[-1])  # Get the index for the variable in the current scope
        self.scopes[-1][name] = (self.level - 1, index)  # Store it in the scope
        self.emit("push 1")
        self.emit("oframe")
        return self.level , index
    
    def lookup_variable(self, name):
        for lvl in reversed(range(len(self.scopes))):
            if name in self.scopes[lvl]:
                return lvl, self.scopes[lvl][name][1]
        raise Exception(f"Variable '{name}' not found")

    def visit_ASTDeclarationNode(self, node):
        print(f"Visiting Declaration Node: {node.id.lexeme}")

        level, index = self.declare_variable(node.id.lexeme)
        # Evaluate RHS expression (if exists)
        if node.expr:
            self.visit(node.expr)
        else:
            self.emit("push 0")  # default init

        # Allocate var in current frame
        

        # Store value in memory
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("st")

    def visit_ASTProgramNode(self, node):

        self.emit(".main")  # Emit the .main label at the beginning of the program
        # Start code generation for the program
        print(f"Generating code for program with {len(node.statements)} statements")

        for stmt in node.statements:
            self.visit(stmt)  # visit each statement (this will dispatch to the appropriate node handler)
        
        # Optionally, you can emit some final instructions like program end
        self.emit("halt")  # or some other end-of-program instruction if required

    def visit_ASTBlockNode(self, node):
        self.enter_scope()
        for stmt in node.stmts:  # assumes `statements` is a list of AST nodes
            self.visit(stmt)
        self.exit_scope()


    def visit_ASTAssignmentNode(self, node):
        self.visit(node.expr)
        level, index = self.lookup_variable(node.id.lexeme)
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("st")
    
    def visit_ASTVariableNode(self, node):
        level, index = self.lookup_variable(node.lexeme)
        self.emit(f"push [{index}:{level}]")

    def visit_ASTIntegerNode(self, node):
        self.emit(f"push {node.value}")

    def visit_ASTFloatNode(self, node):
        self.emit(f"push {node.value}")  # floats are stored as-is

    def visit_ASTBooleanNode(self, node):
        self.emit(f"push {1 if node.value else 0}")

    def visit_ASTColourNode(self, node):
        self.emit(f"push {node.value}")

    def visit_ASTAddOpNode(self, node):
        self.visit(node.right)
        self.visit(node.left)
        if node.op == "+":
            self.emit("add")
        elif node.op == "-":
            self.emit("sub")

    def visit_ASTMultiOpNode(self, node):
        self.visit(node.left)
        self.visit(node.right)
        if node.op == "*":
            self.emit("mul")
        elif node.op == "/":
            self.emit("div")

    def visit_ASTRelOpNode(self, node):
        self.visit(node.left)
        self.visit(node.right)

        ops = {
            '<': "le",
            '<=': "lt",
            '>': "ge",
            '>=': "gt",
            '==': "eq\nnot",
            '!=': "eq"
        }
        self.emit(ops[node.op])

    def visit_ASTUnaryNode(self, node):
        self.visit(node.expr)
        self.emit("not")

    def visit_ASTIfNode(self, node):
        # Evaluate the condition
        self.visit(node.expr)
        
        # Push the else block location (will be patched later)
        self.emit("push# PC+0")  # Placeholder
        else_jump_index = len(self.instructions) - 1
        self.emit("cjmp")
        
        # Then block
        for stmt in node.blocks[0].stmts:
            self.visit(stmt)
            
        # If there's an else block, handle it
        if len(node.blocks) == 2:
            # Push jump past else block (will be patched later)
            self.emit("push #PC+0")  # Placeholder
            end_jump_index = len(self.instructions) - 1
            self.emit("jmp")
            
            # Patch the else jump location
            else_location = len(self.instructions)
            self.instructions[else_jump_index] = f"push #PC+{else_location - else_jump_index}"
            
            # Else block
            for stmt in node.blocks[1].stmts:
                self.visit(stmt)
                
            # Patch the end jump location
            end_location = len(self.instructions)
            self.instructions[end_jump_index] = f"push #PC+{end_location - end_jump_index}"
        else:
            # Patch the else jump location (just continue after then block)
            end_location = len(self.instructions)
            self.instructions[else_jump_index] = f"push #PC+{end_location - else_jump_index}"

    def visit_ASTReturnNode(self, node):
        if node.expr:
            self.visit(node.expr)  # Push value to return
        if self.inside_function:
            self.emit("ret")
        else:
            self.emit("halt")  # Ret not allowed in .main

    def visit_ASTWhileNode(self, node):
        # Index where the condition starts
        condition_start_index = len(self.instructions)

        # Emit condition
        self.visit(node.expr)

        # Reserve space for push #PC+X (will be patched)
        self.emit("push #")  # Placeholder for jump target
        cjmp_index = len(self.instructions) - 1
        self.emit("cjmp")

        # Loop body
        for stmt in node.block.stmts:
            self.visit(stmt)

        # Jump back to condition start (corrected offset)
        current_index = len(self.instructions)
        offset_to_condition = current_index - condition_start_index + 2  # +2 = push + jmp
        self.emit(f"push #PC-{offset_to_condition}")
        self.emit("jmp")

        # Patch the forward jump in cjmp
        after_loop_index = len(self.instructions)
        forward_offset = after_loop_index - cjmp_index
        self.instructions[cjmp_index] = f"push #PC+{forward_offset}"



    def visit_ASTWriteNode(self, node):
        for expr in reversed(node.expressions):
            self.visit(expr)
            # self.emit(f"push {expr.value}")
        
        if node.kw == 1:
            self.emit("write")
        elif node.kw ==0:
            self.emit("writebox")


    def visit_ASTFunctionDeclNode(self, node):
        self.emit(f".{node.identifier}")  # label for function
        self.enter_scope()
        self.inside_function = True
        # Add parameters to memory
        if node.formalparams:
            for i, param in enumerate(node.formalparams):
                self.scopes[-1][param] = (self.level - 1, i)
                self.declare_variable(param)

        # Visit body
        for stmt in node.block.stmts:
            self.visit(stmt)

        self.inside_function = False
        self.exit_scope()

    def visit_ASTFunctionCallNode(self, node):
        for arg in node.args:
            self.visit(arg)

        self.emit(f"push .{node.name.lexeme}")  # push function addr
        self.emit(f"call")  # call

    def visit_ASTPrintNode(self, node):
        self.visit(node.expr)
        self.emit("print")

parser = par.Parser(""" 
                    
                 
                    let x:int = 7;  
                    if (x == 6){
                    
                    __print x;
                    x = x-1;
                    }
                    else{
                    __print 3;}

                    
                    
                    
                    
                    
                    
                """)

ast_root = parser.Parse()


type_checker = TypeChecker()
type_checker.visit(ast_root)

if type_checker.errors:
        
    print("Type checking failed with the following errors:")
    for error in type_checker.errors:
        print(f"- {error}")
else:
    print("Type checking passed!")


generator = CodeGenVisitor()
generator.visit(ast_root)

print("\nGenerated Assembly-like Code:")
code = "\n".join(generator.instructions)
print(code)
pyperclip.copy(code)