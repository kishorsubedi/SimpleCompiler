#!/usr/bin/env python

import sys

import il
from sile_types import types
from il import IlGenException
from freevars import freevars
from symbol_table import SymbolTable

## This file is the intermediate code generator. You likely will
## need to make changes in this file!

## Some portions of the code generator will be working but others
## will not be. You will need to fill it in.

def generate(ast): #111
    mods = il.Modules()
    generate_module(mods, 'main', ast)
    return mods

def generate_module(modules, name, ast): #2222 empty modules = {} , 'main', typed_ast passed
    #print(ast.children[0].children[0].children[1])
    mod = il.Module(name) 
    #print(mod) #module main = {} 
    modules.add(mod)
    #print(modules) #modules = { module main = {} } 
    return IlGenerator.generate(modules, mod, ast)

class IlGenerator(object):

    @staticmethod
    def generate(modules, module, ast): #3333
        self = IlGenerator(modules, module)
        self.stmts(self.function().new_block(), ast)
        #print(self.module)
        return self.module

    def __init__(self, modules, module):
        self.modules = modules
        self.module = module
        self.functions = [self.module.new_function('main', types.Function([], types.UNIT))]
        #print(self.functions)
        self.symbols = SymbolTable()

    def function(self):
        return self.functions[-1]

    def push_function(self, name, type, params, freevars):
        fn = self.module.new_function(
            name, type, params=params, parent=self.function().id, freevars=freevars)
        self.functions.append(fn)
        return fn

    def pop_function(self):
        return self.functions.pop()

    def new_register(self, regtype):
        return self.function().new_register(regtype)

    def stmts(self, blk, n): #blk=funtion block, n= typedast #4444
        
        self.symbols = self.symbols.push()
        for kid in n.children:
            blk = self.stmt(blk, kid)  
            
        self.symbols = self.symbols.pop()
        return blk

    def stmt(self, blk, n): #5555  if and print 
        
        return self.dispatch_stmt(blk, n, {
            "decl": self.decl_action,
            "assign": self.assign_action,
            "expr-stmt": self.expr_stmt_action,
            "print": self.print_action,
            "if": self.if_action,
            "while": self.while_action,
            "label": self.label_action,
            "break": self.break_action,
            "continue": self.continue_action,
            "function": self.function_action,
            "return": self.return_action,
            "stmts": self.stmts,
        })
    

    def decl_action(self, blk, n):
        name = n.children[0].value
        dest = self.new_register(n.children[1].type)
        a, blk = self.expr(blk, dest, n.children[1])
        self.symbols[name] = a
        return blk

    def assign_action(self, blk, n):
        name = n.children[0].value
        dest = self.symbols[name]
        a, blk = self.expr(blk, dest, n.children[1])
        return blk

    def print_action(self, blk, n):     
        a, blk = self.expr(blk, None, n.children[0])
        blk.append(il.Instruction(il.OPS['PRINT'], a, None, None))
        return blk

    def return_action(self, blk, n):
        a, blk = self.expr(blk, None, n.children[0])
        if isinstance(a, il.FunctionRef):
            closure = self.module.lookup(a).closure(self.module, self.symbols)
            if len(closure.captured) > 0:
                a, blk = self.create_closures(blk, closure)
        blk.append(il.Instruction(il.OPS['RTRN'], a, None, None))
        return blk

    def create_closures(self, blk, closure):
        ## Implements function closure creation
        registers = list()
        rewrite = dict()
        for name, operand in closure.captured.iteritems():
            if isinstance(operand, il.Closure):
                fn_ref = operand.fn
                operand, blk = self.create_closures(blk, operand)
                rewrite[fn_ref] = il.ClosureRegister(len(registers), fn_ref.type())
            registers.append(operand)
        result = self.new_register(closure.fn.type())
        rewrite[closure.fn] = il.ClosureRegister(len(registers), closure.fn.type())
        for idx, reg in enumerate(registers):
            rewrite[reg] = il.ClosureRegister(idx, reg.type())
        closure_code = self.rewrite(closure.fn, rewrite)
        blk.append(il.Instruction(
            il.OPS['CLOSURE'], closure_code.ref(), registers, result))
        return result, blk

    def rewrite(self, fn_ref, rewrites):
        ## Implements function rewriting for closure creation
        old = self.module.lookup(fn_ref)
        new = self.push_function(old.name + '-closure', old.type(), old.params, list())
        new.locals = old.locals
        def replace(operand):
            if isinstance(operand, il.FunctionRef) and operand in rewrites:
                return rewrites[operand]
            elif isinstance(operand, il.Register) and operand in rewrites:
                return rewrites[operand]
            elif isinstance(operand, il.Register) and operand.fn == old.ref():
                return il.Register(operand.id, new.ref(), operand.type())
            elif isinstance(operand, list):
                return [replace(inner) for inner in operand]
            return operand
        for old_blk in old.blocks:
            new_blk = new.new_block()
            for inst in old_blk.code:
                new_blk.append(il.Instruction(
                    inst.op, replace(inst.a), replace(inst.b),
                    replace(inst.result)))
        for idx, old_blk in enumerate(old.blocks):
            new_blk = new.blocks[idx]
            for link in old_blk.next:
                new_blk.link_to(new.blocks[link.target], link.link_type)
        self.pop_function()
        return new

    def if_action(self, entry, n): #function block and if node(or print node afterwards) refactoring needed here
        
        a_in_blk = entry 
        if(len(n.children) == 2): #if cond stmt

            a, a_out_blk = self.expr(a_in_blk, None, n.children[0]) #check 1st expression, return block, a==condition 
            exit_blk = self.function().new_block() #4
            b_in_blk = self.function().new_block() #5
            
            a_out_blk.if_link(a, b_in_blk , exit_blk) 
            #                    on-true   on-false
            b_out_blk = self.stmt(b_in_blk, (n.children[1]))
            b_out_blk.goto_link(exit_blk)

            return exit_blk
        else: #if cond stmt else stmt/block
            a, a_out_blk = self.expr(a_in_blk, None, n.children[0]) #check 1st expression, return block, a==condition
            
            b_in_blk = self.function().new_block()
            exit_blk = self.function().new_block()
            else_blk = self.function().new_block()

            a_out_blk.if_link(a, b_in_blk , exit_blk)
            
            #                    on-true   on-false
            b_out_blk = self.stmt(b_in_blk, (n.children[1]))
            b_out_blk.goto_link(else_blk)
            
            
            final_blk = self.stmt(exit_blk, n.children[2])
            
            final_blk.goto_link(else_blk)
            
            return else_blk
            

    def while_action(self, entry, n):
        header = self.function().new_block()
        body = self.function().new_block()
        afterwards = self.function().new_block()
        return self._while_action(entry, n, header, body, afterwards)

    def _while_action(self, entry, n, header, body, afterwards):
        self.function().push_loop(header, afterwards)
        
        entry.goto_link(header)
        cond, cond_out = self.expr(header, None, n.children[0])
        cond_out.if_link(cond, body, afterwards)
        body_out = self.stmts(body, n.children[1])
        body_out.goto_link(header)
        self.function().pop_loop()
        return afterwards

    def label_action(self, entry, n):
        cont_blk = self.function().new_block()
        exit_blk = self.function().new_block()
        return self._label_action(
            entry, n.children[0].value, n.children[1], cont_blk, exit_blk
        )
    
    def _label_action(self, entry, name, stmt, cont_blk, exit_blk):
        self.function().add_label(name, cont_blk, exit_blk)
        self.symbols[name] = self.function().labels[name]
        entry.goto_link(cont_blk)
        afterwards = self.stmt(cont_blk, stmt)
        afterwards.goto_link(exit_blk)
        return exit_blk

    def break_action(self, blk, n):
        if len(n.children) == 0:
            header = self.function().loop_exit()
        else:
            label = n.children[0].value
            header = self.symbols[label].exit_blk
        blk.goto_link(header)
        dead = self.function().new_block()
        return dead
            
    def continue_action(self, blk, n):
        if len(n.children) == 0:
            header = self.function().loop_cont()
        else:
            label = n.children[0].value
            header = self.symbols[label].continue_blk
        blk.goto_link(header)
        dead = self.function().new_block()
        return dead

    def expr_stmt_action(self, blk, n):
        _, blk = self.expr(blk, None, n.children[0])
        return blk

    def function_action(self, blk, n):
        type = n.children[0].type
        name = n.children[0].value
        params = [
            il.Param(id=idx, name=param.children[0].value, type=param.type)
            for idx, param in enumerate(n.children[1].children)]
        body = n.children[3]
        free = freevars(n)
        fn = self.push_function(name, type, params, free)
        self.symbols[name] = fn.ref()
        self.symbols = self.symbols.push()
        entry_blk = fn.new_block()
        for idx, param in enumerate(fn.params):
            r = self.new_register(param.type)
            self.symbols[param.name] = r
            entry_blk.append(il.Instruction(il.OPS['PRM'], param, 0, r))
        self.stmts(entry_blk, body)
        self.symbols = self.symbols.pop()
        self.pop_function()
        self.symbols[name] = fn.ref()
        return blk
        

    def expr(self, blk, result, n):#777
        return self.dispatch_expr(blk, result, n, {
            "negate": self.negate,
            "+": self.binop(il.OPS['ADD']),
            "-": self.binop(il.OPS['SUB']),  
            "*": self.binop(il.OPS['MUL']),
            "/": self.binop(il.OPS['DIV']),
            "==": self.binop(il.OPS['EQ']),
            "!=": self.binop(il.OPS['NE']),
            "<": self.binop(il.OPS['LT']),
            ">": self.binop(il.OPS['GT']),
            "<=": self.binop(il.OPS['LE']),
            ">=": self.binop(il.OPS['GE']),
            "%": self.binop(il.OPS['MOD']),
            "not": self.not_op,
            "&&": self.and_op,
            "||": self.or_op,
            "call": self.call,
            "NAME": self.name,
            "INTEGER": self.number,
            "FLOAT": self.number,
        })

    def negate(self, blk, result, n):
        if result is None:
            result = self.new_register(n.type)
        a, blk = self.expr(blk, None, n.children[0])
        blk.append(il.Instruction(
            il.OPS['SUB'], il.Constant(0, n.type), a, result))
        return result, blk

    def binop(self, op):#8888 I am here #op= "==", children[0] =1 children[1] = 2

        def binop(blk, result, n):
            if result is None:
                result = self.new_register(n.type)
            a, blk = self.expr(blk, None, n.children[0])
            b, blk = self.expr(blk, None, n.children[1])
            blk.append(il.Instruction(op, a, b, result))
            return result, blk
        return binop

    def not_op(self, blk, result, n):
        
        if result is None:
            result = self.new_register(n.type) 
        a, a_out_blk = self.expr(blk, result, n.children[0]) #aoutblk has EQ 
        a_out_blk.append(il.Instruction(il.OPS['NOT'], a, None, result))
        return result, a_out_blk
        

    def and_op(self, a_in_blk, result, n):
        
        if result is None:
            result = self.new_register(n.type) 
        a, a_out_blk = self.expr(a_in_blk, result, n.children[0]) #n.children[0] == boolean
        b_in_blk = self.function().new_block()
        exit_blk = self.function().new_block()
        a_out_blk.if_link(a, b_in_blk, exit_blk)
        #                    on-true   on-false
        b, b_out_blk = self.expr(b_in_blk, result, n.children[1])
        b_out_blk.goto_link(exit_blk)
        return result, exit_blk
        

    def or_op(self, a_in_blk, result, n): #empty block, None, || node
        #print(n.type)
        if result is None:
            result = self.new_register(n.type) 
        a, a_out_blk = self.expr(a_in_blk, result, n.children[0]) #check 1st expression, return block, a==condition
        b_in_blk = self.function().new_block()
        exit_blk = self.function().new_block()
        
        a_out_blk.if_link(a, exit_blk, b_in_blk)
        #                    on-true   on-false

        b, b_out_blk = self.expr(b_in_blk, result, n.children[1])

        b_out_blk.goto_link(exit_blk)
      
        return result, exit_blk

    def call(self, blk, result, n):
        if result is None:
            result = self.new_register(n.type)
        #fn, blk = ## TODO: populate these variables
        param_operands = list()
        ## TODO: POPULATE PARAMS
        blk.append(il.Instruction(il.OPS['CALL'], fn, param_operands, result))
        return result, blk

    def name(self, blk, result, n):
        if result is None:
            return self.symbols[n.value], blk
        blk.append(il.Instruction(
            il.OPS['MV'], self.symbols[n.value], None, result))
        return result, blk

    def number(self, blk, result, n):
        const = il.Constant(n.value, n.type)
        if result is None:
            return const, blk
        blk.append(il.Instruction(il.OPS['IMM'], const, None, result))
        return result, blk

    def dispatch_stmt(self, blk, n, labels_to_actions):
        for name, func in labels_to_actions.iteritems():
            if n.label == name:
                return func(blk, n)
        raise IlGenException(
            "got '{}', want one of: {}".format(n.label, labels_to_actions.keys()))

    def dispatch_expr(self, blk, result, n, labels_to_actions):
        for name, func in labels_to_actions.iteritems():
            if n.label == name:
                return func(blk, result, n)
        raise IlGenException(
            "got '{}', want one of: {}".format(n.label, labels_to_actions.keys()))

