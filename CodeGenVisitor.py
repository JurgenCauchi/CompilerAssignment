import parserblock as par
from SemanticVisitor import Visitor
from SemanticVisitor import TypeChecker
import astnodes_ as ast
import pyperclip


class CodeGenVisitor(Visitor):
    def __init__(self):
        self.instructions = []
        # memory stack (SoF), stores (level, index) for each variable
        self.scopes = [{}]
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
        self.scopes.insert(0, {})
        for scope in self.scopes:
            for var in scope:
                index, level = scope[var]
                scope[var] = (index, level + 1)

    def exit_scope(self):
        for scope in self.scopes:
            for var in scope:
                index, level = scope[var]
                scope[var] = (index, level - 1)
        self.scopes.pop(0)

    def declare_variable(self, name):
        scope = self.scopes[0]
        idx = len(scope)
        scope[name] = (idx, self.level)
        print(self.scopes)
        return self.level, idx

    def lookup_variable(self, name):
        for scope in (self.scopes):
            if name in scope:
                idx, level = scope[name]
                return idx, level
        raise Exception(f"Variable '{name}' not found")

    def visit_ASTDeclarationNode(self, node):
        level, index = self.declare_variable(node.id.lexeme)

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

        # Emit the .main label at the beginning of the program
        self.emit(".main")
        self.emit("push 4")
        self.emit("jmp")
        self.emit("halt")
        spacesneeded = sum(1 for stmt in node.statements if isinstance(
            stmt, ast.ASTDeclarationNode))
        self.emit(f"push {spacesneeded}")
        self.emit("oframe")
        # Start code generation for the program
        print(
            f"Generating code for program with {len(node.statements)} statements")

        for stmt in node.statements:
            # visit each statement (this will dispatch to the appropriate node handler)
            self.visit(stmt)
        for i in range(len(self.scopes)):
            self.emit("cframe")
        # or some other end-of-program instruction if required
        self.emit("halt")

    def visit_ASTBlockNode(self, node):
        self.enter_scope()
        for stmt in node.stmts:  # assumes `statements` is a list of AST nodes
            self.visit(stmt)
        self.exit_scope()

    def visit_ASTAssignmentNode(self, node):
        self.visit(node.expr)
        index, level = self.lookup_variable(node.id.lexeme)
        self.emit(f"push {index} //Start of assignment")
        self.emit(f"push {level}")
        self.emit("st")

    def visit_ASTVariableNode(self, node):
        index, level = self.lookup_variable(node.lexeme)
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
        self.visit(node.right)
        self.visit(node.left)
        print(node.op)
        if node.op == "*" or node.op == "and":
            self.emit("mul")
        elif node.op == "/":
            self.emit("div")

    def visit_ASTRelOpNode(self, node):
        self.visit(node.right)
        self.visit(node.left)

        ops = {
            '<': "lt",
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

        self.emit("push #PC+4")  # Placeholder
        self.emit("cjmp")
        # Push the else block location (will be patched later)
        else_jump_index = len(self.instructions)
        self.emit("push #PC+0")  # Placeholder
        self.emit("jmp")

        # Then block
        num_vars = sum(1 for stmt in node.blocks if isinstance(
            stmt, ast.ASTDeclarationNode))  # or stmt.type == 'var_decl'
        self.emit(f"push {num_vars}")
        self.emit("oframe")
        self.enter_scope()
        for stmt in node.blocks[0].stmts:
            self.visit(stmt)
        self.exit_scope()
        self.emit("cframe")
        # If there's an else block, handle it
        if len(node.blocks) == 2:

            # Push jump past else block (will be pa+tched later)
            end_jump_index = len(self.instructions)
            self.emit("push #PC+0")  # Placeholder
            self.emit("jmp")
            num_vars = sum(1 for stmt in node.blocks if isinstance(
                stmt, ast.ASTDeclarationNode))  # or stmt.type == 'var_decl'
            self.emit(f"push {num_vars}")
            self.emit("oframe")
            # Patch the else jump location
            else_location = len(self.instructions) - 2
            self.instructions[else_jump_index] = f"push #PC+{else_location - else_jump_index}"

            self.enter_scope()
            # Else block
            for stmt in node.blocks[1].stmts:
                self.visit(stmt)
            self.exit_scope()
            self.emit("cframe")
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

        self.emit("push #PC+4")
        self.emit("cjmp")
        # Reserve space for push #PC+X (will be patched)

        cjmp_index = len(self.instructions)
        self.emit("push #")  # Placeholder for jump target
        self.emit("jmp")

        num_vars = sum(1 for stmt in node.block.stmts if isinstance(
            stmt, ast.ASTDeclarationNode))  # or stmt.type == 'var_decl'
        self.emit(f"push {num_vars}")
        self.emit("oframe")

        self.enter_scope()
        # Loop body
        for stmt in node.block.stmts:
            self.visit(stmt)
        self.exit_scope()
        self.emit("cframe")

        # Jump back to condition start (corrected offset)
        current_index = len(self.instructions)
        offset_to_condition = current_index - condition_start_index  # +2 = push + jmp
        after_loop_index = len(self.instructions) + 2
        self.emit(f"push #PC-{offset_to_condition}")
        self.emit("jmp")

        # Patch the forward jump in cjmp

        forward_offset = after_loop_index - cjmp_index
        self.instructions[cjmp_index] = f"push #PC+{forward_offset}"

    def visit_ASTForNode(self, node):
        # Enter a new scope for the loop
        # Initialization
        spacesneeded = len(self.scopes[0])
        self.emit(f"push {spacesneeded}")
        self.emit("oframe")
        self.enter_scope()
        if node.vardec:
            self.visit(node.vardec)

        # Index where the condition starts
        condition_start_index = len(self.instructions)

        # Condition (optional, if exists)
        if node.expr:
            self.visit(node.expr)
            self.emit("push #PC+4")
            self.emit("cjmp")
            # Reserve space for push #PC+X (to be patched)
            cjmp_index = len(self.instructions)
            self.emit("push #")  # Placeholder for jump target
            self.emit("jmp")
        else:
            cjmp_index = None  # No condition to jump on

        num_vars = sum(1 for stmt in node.blck.stmts if isinstance(
            stmt, ast.ASTDeclarationNode))  # or stmt.type == 'var_decl'
        self.emit(f"push {num_vars}")
        self.emit("oframe")
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
        offset_to_condition = current_index - condition_start_index
        self.emit(f"push #PC-{offset_to_condition}")
        self.emit("jmp")

        # Patch the conditional jump if there was a condition
        if cjmp_index is not None:
            after_loop_index = len(self.instructions)
            forward_offset = after_loop_index - cjmp_index
            self.instructions[cjmp_index] = f"push #PC+{forward_offset}"

         # Exit the loop scope
        self.exit_scope()

    def visit_ASTWriteNode(self, node):
        for expr in reversed(node.expressions):
            self.visit(expr)
            # self.emit(f"push {expr.value}")

        if node.kw == 1:
            self.emit("write")
        elif node.kw == 0:
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
        print(node.formalparams)
        for param in node.formalparams:
            self.declare_variable(param[0])

        # body
        for stmt in node.block.stmts:
            self.visit(stmt)

        # ensure return
        if not any(instr.startswith("ret") for instr in self.instructions[-3:]):
            self.emit("push 0")
            self.emit("ret")

        self.emit("cframe")  # clean up the frame
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

# parser = par.Parser("""


# let c:colour = 0 as colour;

#  for (let i:int = 0; i < 4; i = i + 1) {
#  c = __random_int 1677216 as colour;
#  __clear c;

#  __delay 1000;
#  }
#                 """)

parser = par.Parser(""" 


let h:int = __height - 1;
let w:int = __width - 1;
let c:colour = #bbbbbb;


    for (let i:int = 0; i < h; i = i + 1) {  
        __write w,i,c;
        __delay 16;
                                                  
            }
     h = h - 1;               
    for (let j:int = w; j < 0; j = j - 1) {
        __write j,h,c;
        __delay 16;

    }       
     w = w -1;        
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
