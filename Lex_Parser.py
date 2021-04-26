# LEX_Parser.py by Gabriel Ortega and Paulina Cámara (2021)
# Lexer and Parser program using ply

import ply.lex as lex
import ply.yacc as yacc
import sys
from datastruct import DirProcess
from collections import deque
from ObjQuad import Quadruple

pilaO = deque()
poper = deque()
ptypes = deque()
dic = DirProcess()

global_int = 0
global_float = 1000
global_char = 2000
global_bool = 3000

quadruples = []

# ***** LEXER *****
# Tokens
tokens = [

    'PROGRAM',      #program
    'ID',           #id
    'CLASS',        #Class
    'INHERIT',      #Inherit
    'ATTRIBUTES',   #Attributes
    'METHODS',      #Methods
    'VARS',         #Global variables(vars)
    'VAR',          #variables (var)
    'FUNCTION',      #Function
    'MAIN',         #Main
    'READ',         #Read
    'WRITE',        #Write
    'RETURN',       #Return
    'IF',           #if
    'THEN',         #then
    'ELSE',         #else
    'VOID',         #Void
    'WHILE',        #While
    'DO',           #Do
    'FROM',         #From
    'TO',           #To
    'LESS',         #<
    'GREATER',      #>
    'SEMICOLON',    #;
    'COMMA',        #,
    'COLON',        #:
    'DOT',          #.
    'L_CURPAR',     #{
    'R_CURPAR',     #}
    'L_BREAK',      #[
    'R_BREAK',      #]
    'L_PAR',        #(
    'R_PAR',        #)
    'INT',          #int
    'FLOAT',        #float
    'CHAR',         #char
    'CTE_INT',      #Cte.int
    'CTE_FLOAT',    #Cte.float
    'CTE_CHAR',     #Cte.char
    'EQUAL',        #=
    'PLUS',         #+
    'MINUS',        #-
    'MULT',         #*
    'DIV',          #/
    'AND',          #&
    'OR',           #|
    'PLUS_EQ',      #+=
    'MIN_EQ',       #-=
    'MULT_EQ',      #*=
    'DIV_EQ',       #/=
    'LESS_TH',      #<=
    'GREAT_TH',     #>=
    'DIF',          #!=
    'SAME'          #==
]

# Definition of tokens
t_LESS = r'\<'          #<
t_GREATER = r'\>'       #>
t_SEMICOLON = r'\;'     #;
t_COMMA = r'\,'         #,
t_COLON = r'\:'         #:
t_DOT = r'\.'           #.
t_L_CURPAR = r'\{'      #{
t_R_CURPAR = r'\}'      #}
t_L_BREAK = r'\['       #[
t_R_BREAK = r'\]'       #]
t_L_PAR = r'\('         #(
t_R_PAR = r'\)'         #)
t_EQUAL = r'\='         #=
t_PLUS = r'\+'          #+
t_MINUS = r'\-'         #-
t_MULT = r'\*'          #*
t_DIV = r'\/'           #/
t_AND = r'\&'            #&
t_OR = r'\|'            #|
t_PLUS_EQ = r'\+\='     #+=
t_MIN_EQ = r'\-\='      #-=
t_MULT_EQ = r'\*\='     #*=
t_DIV_EQ = r'\/\='      #/=
t_LESS_TH = r'\<\='     #<=
t_GREAT_TH = r'\>\='    #>=
t_DIF = r'\=\='         #!=
t_SAME = r'\=\='        #==

t_ignore = ' \t'

#Definition of complex tokens
def t_PROGRAM(t):
    r'program'
    t.type = 'PROGRAM'
    return t

def t_CLASS(t):
    r'class'
    t.type = 'CLASS'
    return t 

def t_INHERIT(t):
    r'inherit'
    t.type = 'INHERIT'
    return t 

def t_ATTRIBUTES(t):
    r'attributes'
    t.type = 'ATTRIBUTES'
    return t 

def t_METHODS(t):
    r'methods'
    t.type = 'METHODS'
    return t 

def t_VARS(t):
    r'vars'
    t.type = 'VARS'
    return t 

def t_FUNCTION(t):
    r'function'
    t.type = 'FUNCTION'
    return t

def t_MAIN(t):
    r'main'
    t.type = 'MAIN'
    return t

def t_INT(t):
    r'int'
    t.type = 'INT'
    return t

def t_FLOAT(t):
    r'float'
    t.type = 'FLOAT'
    return t

def t_CHAR(t):
    r'char'
    t.type = 'CHAR'
    return t

def t_READ(t):
    r'read'
    t.type = 'READ'
    return t 

def t_WRITE(t):
    r'write'
    t.type = 'WRITE'
    return t 

def t_RETURN(t):
    r'return'
    t.type = 'RETURN'
    return t 

def t_VOID(t):
    r'void'
    t.type = 'VOID'
    return t 

def t_WHILE(t):
    r'while'
    t.type = 'WHILE'
    return t 

def t_DO(t):
    r'do'
    t.type = 'DO'
    return t 

def t_FROM(t):
    r'from'
    t.type = 'FROM'
    return t

def t_TO(t):
    r'to'
    t.type = 'TO'
    return t

def t_IF(t):
    r'if'
    t.type = 'IF'
    return t

def t_THEN(t):
    r'then'
    t.type = 'THEN'
    return t

def t_ELSE(t):
    r'else'
    t.type = 'ELSE'
    return t 

def t_VAR(t):
    r'[a-z][a-zA-Z_0-9]*'
    t.type = 'VAR'
    return t

def t_ID(t):
    r'[A-Z][a-zA-Z0-9]*'
    t.type = 'ID'
    return t

def t_CTE_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_CTE_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_CTE_CHAR(t):
    r"'[a-zA-Z0-9!@#$%^&*()]'"
    t.value = str(t.value)
    return t

#Function to count lines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

#Function to add comments
def t_comment(t):
    r'\#.*'
    pass

#Function to show lexical error 
def t_error(t):
    print('Line: %d, Not valid character: %r' % (t.lexer.lineno, t.value[0]))
    t.lexer.skip(1)

lexer = lex.lex()

# Test scanner function
"""def pruebaLex():
    lexer.input('if else print + - "HOLA" program vars 123 123.1 -123.5 class * / gabo_125 Gabo')

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

pruebaLex()"""

# ***** SEMANTIC CUBE *****

semanticCube = {
    'int':{
        'int':{
            '-': 'int',
            '*': 'int',
            '/': 'int',
            '-=': 'int',
            '*=': 'int',
            '/=': 'int',
            '+': 'int',
            '+=': 'int',
            '<': 'bool',
            '>': 'bool',
            '<=': 'bool',
            '>=': 'bool',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'float':{
            '-': 'float',
            '*': 'float',
            '/': 'float',
            '-=': 'float',
            '*=': 'float',
            '/=': 'float',
            '+': 'float',
            '+=': 'float',
            '<': 'bool',
            '>': 'bool',
            '<=': 'bool',
            '>=': 'bool',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'char':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'bool':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        }
    },
    'float':{
        'int':{
            '-': 'float',
            '*': 'float',
            '/': 'float',
            '-=': 'float',
            '*=': 'float',
            '/=': 'float',
            '+': 'float',
            '+=': 'float',
            '<': 'bool',
            '>': 'bool',
            '<=': 'bool',
            '>=': 'bool',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'float':{
            '-': 'float',
            '*': 'float',
            '/': 'float',
            '-=': 'float',
            '*=': 'float',
            '/=': 'float',
            '+': 'float',
            '+=': 'float',
            '<': 'bool',
            '>': 'bool',
            '<=': 'bool',
            '>=': 'bool',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'char':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'bool':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        }
    },
    'char':{
        'int':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'float':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'char':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'char',
            '+=': 'char',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'bool':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        }
    },
    'bool':{
        'int':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'float':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'char':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'char',
            '+=': 'char',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error'
        },
        'bool':{
            '-': 'error',
            '*': 'error',
            '/': 'error',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'error',
            '+=': 'error',
            '<': 'error',
            '>': 'error',
            '<=': 'error',
            '>=': 'error',
            '!=': 'bool',
            '==': 'bool',
            '&': 'bool',
            '|': 'bool'
        }
    }
}

# ***** PARSER *****

# Grammar definition
def p_program(p):
    '''
    program : PROGRAM ID SEMICOLON programT
    '''
    dic.add_prog(p[2])
    #print(p[2])
    p[0] = None

def p_programT(p):
    '''
    programT : class programT
             | vars programF
             | programF
    
    programF : func programF 
             | main empty
    '''
    p[0] = None

def p_class(p):
    '''
    class : CLASS ID classT
    '''
    # print(p[2])
    p[0] = None

def p_classT(p):
    '''
    classT : LESS INHERIT ID GREATER classF
            | classF
    
    classF : SEMICOLON L_CURPAR ATTRIBUTES dec METHODS func R_CURPAR SEMICOLON empty
    '''
    p[0] = None

def p_vars(p):
    '''
    vars  : VARS dec empty
    '''
    dic.add_process(p[1]+":"+p[2])
    #print(p[1]+":"+p[2])
    p[0] = None

def p_dec(p):
    '''
    dec : VAR arr decF  
        | VAR decF 
    '''
    if len(p) == 4:
      p[0] = p[1]+p[2]+"."+p[3]
    elif p[2] != None:
      p[0] = p[1]+"."+p[2]

def p_decF(p):
    ''' 
    decF : COMMA dec
          | COLON type SEMICOLON dec
          | COLON type SEMICOLON empty
    '''
    if p[1] == ",":
      p[0] = p[2]
    elif p[4] == None:
      p[0] = "("+p[2]+")"
    else:
      p[0] = "("+p[2]+")." + p[4]

def p_type(p):
    '''
    type  : INT empty
          | FLOAT empty
          | CHAR empty
          | ID empty
    '''
    p[0] = p[1]

def p_arr(p):
    '''
    arr : L_BREAK CTE_INT COMMA CTE_INT R_BREAK empty
        | L_BREAK CTE_INT R_BREAK  empty 
    '''
    if p[3] == ",":
      p[0] = "[" + str(p[2]) + "," + str(p[4]) + "]"
    else:
      p[0] = "[" + str(p[2]) + "]"

def p_func(p):
    '''
    func : typeFunc FUNCTION ID L_PAR funcF
    '''
    dic.add_process(p[3]+"("+p[1]+")" +p[5])
    #print(p[3]+"("+p[1]+")" +p[5])
    p[0] = None

def p_funcF(p):
    '''
    funcF : parameter R_PAR SEMICOLON dec L_CURPAR statement R_CURPAR empty
           | R_PAR SEMICOLON dec L_CURPAR statement R_CURPAR empty
    '''
    if len(p) == 9:
      p[0] = ":"+p[4]
    else:
      p[0] = ":"+p[3]

def p_typeFunc(p):
    '''
    typeFunc  : INT empty
              | FLOAT empty
              | CHAR empty
              | ID empty
              | VOID empty
    '''
    p[0] = p[1]


def p_paramater(p):
    '''
    parameter : VAR COLON typepar SEMICOLON parameterF
    
    parameterF : parameter
                 | empty
    '''
    p[0] = None

def p_typepar(p):
    '''
    typepar  : INT empty
          | FLOAT empty
          | CHAR empty
          | ID empty
    '''
    p[0] = None

def p_main(p):
    '''
    main : MAIN L_PAR R_PAR L_CURPAR statement R_CURPAR empty
    '''
    p[0] = None

def p_statement(p):
    '''
    statement : assigment statementF
                | void statementF
                | return statementF
                | read statementF
                | write statementF
                | if statementF
                | repeat statementF

    statementF : statement
                | empty
    '''
    p[0] = None

def p_void(p):
    '''
    void : ID  DOT ID L_PAR param R_PAR SEMICOLON empty
            | ID L_PAR param R_PAR SEMICOLON empty
            | ID L_PAR R_PAR SEMICOLON empty
    '''
    p[0] = None

def p_arrfunc(p):
    '''
    arrfunc : L_BREAK exp COMMA exp R_BREAK empty
            | L_BREAK exp R_BREAK empty
    '''
    p[0] = None

def p_param(p):
    '''
    param : exp paramF

    paramF : COMMA param
            | empty
    '''
    p[0] = None

def p_return(p):
    '''
    return : RETURN L_PAR exp R_PAR SEMICOLON empty
    '''
    p[0] = None

def p_var(p):
    '''
    var : VAR varF  
        | ID DOT VAR varF  
    '''
    if len(p) == 3:
        p[0] = p[1]

def p_varF(p):
    '''
    varF : arrfunc empty
          | empty
    '''

def p_read(p):
    '''
    read : READ L_PAR readF
    
    readF : var COMMA readF 
          | var R_PAR SEMICOLON empty
    '''
    p[0] = None

def p_write(p):
    '''
    write  : WRITE L_PAR writeT

    writeT : CTE_CHAR writeF
            | exp writeF

    writeF : COMMA  writeT
               | R_PAR SEMICOLON empty
    '''
    p[0] = None

def p_repeat(p):
    '''
    repeat : conditional empty
            | nonconditional empty
    '''
    p[0] = None

def p_if(p):
    '''
    if : IF L_PAR exp R_PAR THEN L_CURPAR statement SEMICOLON R_CURPAR ifF

    ifF : ELSE L_CURPAR statement SEMICOLON R_CURPAR empty
        | empty
    '''
    p[0] = None

def p_assigment(p):
    '''
    assigment : var assigmentF

    assigmentF : EQUAL exp empty 
                | ope exp empty
    '''
    p[0] = None

def p_ope(p):
    '''
    ope : PLUS_EQ empty
        | MIN_EQ empty
        | MULT_EQ empty
        | DIV_EQ empty
    '''
    p[0] = None

def p_conditional(p):
    '''
    conditional : WHILE L_PAR exp R_PAR DO L_CURPAR statement SEMICOLON R_CURPAR empty
    '''
    p[0] = None

def p_nonconditional(p):
    '''
    nonconditional : FROM VAR arrfunc nonconditionalF
                    | FROM VAR nonconditionalF

    nonconditionalF : exp TO exp DO L_CURPAR statement SEMICOLON R_CURPAR empty
    '''
    p[0] = None

def p_bool(p):
    '''
    bool : OR exp empty
        | AND exp empty
    '''
    p[0] = None

def p_exp(p):
    '''
    exp : ex expT

    expT : LESS expf
          | GREATER expf
          | LESS_TH expf
          | GREAT_TH expf
          | SAME expf
          | DIF expf
          | empty

    expf : ex empty
          | ex bool empty
    '''
    p[0] = None


def p_ex(p):
    '''
    ex  : term np_addEx exF

    exF : PLUS np_addOp ex
         | MINUS np_addOp ex
         | empty
    '''
    p[0] = None

def p_term(p):
    '''
    term : factor np_addTerm termF

    termF : MULT np_addOp term
         | DIV np_addOp term
         | empty
    '''
    p[0] = None

def p_factor(p):
    '''
    factor  : L_PAR np_addPar exp R_PAR np_popPar empty
            | factorT

    factorT : PLUS factorF
            | MINUS factorF
            | factorF

    factorF : varcte empty
    '''
    p[0] = None

def p_varcte(p):
    '''
    varcte  : var np_addId empty
            | CTE_INT empty
            | CTE_FLOAT empty
            | CTE_CHAR empty
    '''
    p[0] = None


#Error function
def p_error(p):
    print("Syntax error found at line %d." % (lexer.lineno))
    print(p)

#Empty function
def p_empty(p):
    '''
    empty : 
    '''
    p[0] = None

# ***** NEURALGIC POINTS *****

def p_np_addId(p):
    'np_addId : '
    pilaO.append(p[-1])
    check_type_id(p[-1])

def check_type_id(check):
    if dic.get_item("Program").vars.is_occupied(check):
        ptypes.append(dic.get_item("Program").vars.get_item(check).Obj_type)

def p_np_addOp(p):
    'np_addOp : '
    poper.append(p[-1])

def p_np_addPar(p):
    'np_addPar : '
    poper.append(p[-1])

def p_np_popPar(p):
    'np_popPar : '
    if poper[-1] != '(':
        print('Error with fake bottom')
        sys.exit()
    else:
        poper.pop()

def p_np_addTerm(p):
    'np_addTerm : '
    generateQuad(['*','/'])

def p_np_addEx(p):
    'np_addEx : '
    generateQuad(['+','-'])

#Function that will generate the quadruples
def generateQuad(check):
    if len(poper) > 0:
        if poper[-1] in check:
            op = poper.pop()
            opdo_der = pilaO.pop()
            opdoT_der = ptypes.pop()
            opdo_izq = pilaO.pop()
            opdoT_izq = ptypes.pop()
            tempType = semanticCube[opdoT_der][opdoT_izq][op]
            if tempType == 'error':
                print('Error trying to generate quadruple')
                sys.exit()
            else:
                temp = generate_temporal(tempType)
                pilaO.append(temp)
                ptypes.append(tempType)
                quadruples.append(Quadruple(op, opdo_izq, opdo_der, temp))

def generate_temporal(tempType):
    global global_int, global_float, global_char, global_bool
    temp = ""
    if tempType == 'int':
        if global_int > 999: 
            error('Limit of variables of type {} reached'.format(tempType))
        temp = "ti" + str(global_int + 1)
        global_int += 1
    elif tempType == 'float':
        if global_float > 1999: 
            error('Limit of variables of type {} reached'.format(tempType))
        temp = "tf" + str(global_float - 999)
        global_float += 1
    elif tempType == 'char':
        if global_char > 2999: 
            error('Limit of variables of type {} reached'.format(tempType))
        temp = "tc" + str(global_char - 1999)
        global_char += 1
    elif tempType == 'bool':
        if global_bool > 3999: 
            error('Limit of variables of type {} reached'.format(tempType))
        temp = "tb" + str(global_bool- 2999)
        global_bool += 1
    return temp

parser = yacc.yacc()

#Test Lex and Parser with txt
#read 1 txt
try:
    text = input('Insert test doc (.txt): ')
    with open(text, 'r') as file:
        parser.parse(file.read())
except EOFError:
    print("Error")

print(pilaO) 
print(poper)
print(ptypes)
for item in quadruples:
    print(item.get_quad())

#insert name and read txt
"""
while True:
    try:
        text = input('Insert test doc (.txt): ')
        f = open(text, "r")
        lexer.lineno = 1
        for s in f:
            parser.parse(s)
    except EOFError:
        print('Error')
"""
