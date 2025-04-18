import parserblock as par
from SemanticVisitor import Visitor
from SemanticVisitor import TypeChecker
import astnodes_ as ast
import pyperclip



class CodeGenVisitor(Visitor):
    def __init__(self):
        self.instructions = []
        self.scopes = [{}]  # memory stack (SoF), stores (level, index) for each variable
        self.level = 0    # level in the SoF (stack of frames)
        self.func_positions = {}         # map function name to its entry index
        self.call_patches = []   

    def visit(self, node):
        method = f"visit_{type(node).__name__}"
        print(f"Visiting node: {type(node).__name__}")
        return getattr(self, method, self.generic_visit)(node)

    def generic_visit(self, node):
        print(f"Unhandled node: {type(node).__name__}")

    def emit(self, instr):
        self.instructions.append(instr)

    def enter_scope(self):
        self.scopes.append({})
        self.level += 1

    def exit_scope(self):
        self.scopes.pop()
        self.level -= 1

    def declare_variable(self, name):
        idx = len(self.scopes[-1])
        #print(f"Declaring {name} at level {self.level}, index {idx}")
        self.scopes[-1][name] = (self.level, idx)
        return self.level, idx

    def lookup_variable(self, name):
        for scope in reversed(self.scopes):
            if name in scope:
                level, idx = scope[name]
                #print(f"Resolved {name} to [{idx}:{level}]")
                return level, idx
        raise Exception(f"Variable '{name}' not found")



    def visit_ASTDeclarationNode(self, node):
        print(f"Visiting Declaration Node: {node.id.lexeme}")

        level, index = self.declare_variable(node.id.lexeme)

        # Allocate space in the frame before storing value
        self.emit("push 1 //Start of variable declaration")
        self.emit("oframe")

        # Evaluate RHS expression or default to 0
        # Evaluate RHS expression or default to 0
        if node.expr:
            self.visit(node.expr)
        else:
            self.emit("push 0")  # First push for default value
            self.emit("push 0 ")  # Possibly type tag or default fallback

        # Store the evaluated value into memory
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("st")



    def visit_ASTProgramNode(self, node):

        self.emit(".main")  # Emit the .main label at the beginning of the program
        self.emit("push 4")
        self.emit("jmp")
        self.emit("halt")
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
        self.emit(f"push {index} //Start of assignment")
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
        self.visit(node.left)
        self.visit(node.right)
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
        self.visit(node.right)
        self.visit(node.left)

        ops = {
            '<': "lr",
            '<=': "le",
            '>': "gt",
            '>=': "ge",
            '==': "eq",
            '!=': "eq\nnot",
        }
        self.emit(ops[node.op])

    def visit_ASTUnaryNode(self, node):
        self.visit(node.expr)
        self.emit("not")

    def visit_ASTIfNode(self, node):
        # Evaluate the condition
        self.visit(node.expr)
        
        # Push the else block location (will be patched later)
        self.emit("push #PC+0")  # Placeholder
        else_jump_index = len(self.instructions) - 1
        self.emit("cjmp")
        
        # Then block
        for stmt in node.blocks[0].stmts:
            self.visit(stmt)
            
        # If there's an else block, handle it
        if len(node.blocks) == 2:
            # Push jump past else block (will be pa+tched later)
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

    def visit_ASTForNode(self, node):
          # Enter a new scope for the loop
        # Initialization
        if node.vardec:
            self.visit(node.vardec)

        # Index where the condition starts
        condition_start_index = len(self.instructions)

        # Condition (optional, if exists)
        if node.expr:
            self.visit(node.expr)

            # Reserve space for push #PC+X (to be patched)
            self.emit("push #")  # Placeholder for jump target
            cjmp_index = len(self.instructions) - 1
            self.emit("cjmp")
        else:
            cjmp_index = None  # No condition to jump on

        self.emit("oframe")  # Before loop body

        # Loop body...
        self.enter_scope()
        for stmt in node.blck.stmts:
            self.visit(stmt)
        self.exit_scope() 

        # After loop ends
        self.emit("cframe")
        # Post-iteration step
        if node.assgn:
            self.visit(node.assgn)

        # Jump back to condition start
        current_index = len(self.instructions)
        offset_to_condition = current_index - condition_start_index + 2  # +2 for push + jmp
        self.emit(f"push #PC-{offset_to_condition}")
        self.emit("jmp")

        # Patch the conditional jump if there was a condition
        if cjmp_index is not None:
            after_loop_index = len(self.instructions)
            forward_offset = after_loop_index - cjmp_index
            self.instructions[cjmp_index] = f"push #PC+{forward_offset}"

         # Exit the loop scope


    def visit_ASTWriteNode(self, node):
        for expr in reversed(node.expressions):
            self.visit(expr)
            # self.emit(f"push {expr.value}")
        
        if node.kw == 1:
            self.emit("write")
        elif node.kw ==0:
            self.emit("writebox")

    def visit_ASTFunctionCallNode(self, node):
        # Push arguments in reverse order
        for param in reversed(node.params):
            self.visit(param)
        
        # Push argument count
        self.emit(f"push {len(node.params)} //Start of function call")
        
        # Push function label
        self.emit(f"push .{node.ident}")
        self.emit(f"call")
        
    def visit_ASTFunctionDeclNode(self, node):
        # jump over function body
        jmp_idx = len(self.instructions)
        self.emit("push #PC+__ ")  # placeholder
        self.emit("jmp")

        # label entry
        entry_idx = len(self.instructions)
        self.emit(f".{node.identifier}")
        self.func_positions[node.identifier] = entry_idx

        # function prologue
        self.enter_scope()
        self.inside_function = True
        param_count = len(node.formalparams)
        self.emit(f"push {param_count}")
        self.emit("alloc")
        for i, param in enumerate(node.formalparams):
            self.scopes[-1][param[0]] = (self.level, i)
            self.emit(f"push {i}")
            self.emit(f"push {self.level}")
            self.emit("st")

        # body
        for stmt in node.block.stmts:
            self.visit(stmt)

        # ensure return
        if not any(instr.startswith("ret") for instr in self.instructions[-3:]):
            self.emit("push 0")
            self.emit("ret")

        self.inside_function = False
        self.exit_scope()

        # patch jump over function
        end_idx = len(self.instructions)
        offset = end_idx - jmp_idx
        self.instructions[jmp_idx] = f"push #PC+{offset}"
    
    # (Matches your example's behavior where return value is used)
    def visit_ASTPrintNode(self, node):
        self.visit(node.expr)
        self.emit("print")

    def visit_ASTDelayNode(self, node):
        self.visit(node.expr)
        self.emit("delay")

    def visit_ASTClearNode(self, node):
        self.visit(node.expr)
        self.emit("clear")


    def visit_ASTPadRandINode(self, node):
        self.visit(node.expr)
        self.emit("irnd")

    def visit_ASTPadWidthNode(self, node):
        self.emit("width")

    def visit_ASTPadHeightNode(self, node):
        self.emit("height")

    def visit_ASTCastNode(self, node):  
        self.visit(node.expr)  

parser = par.Parser(""" 


let c:colour = 0 as colour;

 for (let i:int = 0; i < 64; i = i + 1) {
 c = __random_int 1677216 as colour;
 __clear c;

 __delay 1000;
 }
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
if type_checker.errors:
    print("Type checking failed with the following errors:")
    for error in type_checker.errors:
        print(f"- {error}")
else:
    print("Type checking passed!")
    print("\nGenerated Assembly-like Code:")
    code = "\n".join(generator.instructions)
    print(code)
    pyperclip.copy(code)
