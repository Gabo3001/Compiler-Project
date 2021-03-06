# Lex_Parser.py by Gabriel Ortega and Paulina Cámara (2021)
# Lexer and Parser program using ply

import ply.lex as lex
import ply.yacc as yacc
import sys
from datastruct import DirProcess
from collections import deque
from ObjQuad import Quadruple
from copy import copy
from re import match


dic = DirProcess()
auxiliarDic = DirProcess()

currFunc = ''
currType = ''
progName = ''
arrFunc = ''
paramK = 0
contReturns = 0

const_table = {}

pvars = deque()
pvarsT = deque()
pilaO = deque()
poper = deque()
ptypes = deque()
pjumps = deque()
pdim = deque()
pcalls = deque()
pParams = deque()
pClassCalls = deque()

arrClases = []
quadruples = []
quadaux = []

global_class = 0
local_class = 500
global_int = 1000
global_float = 2000
global_char = 3000
global_bool = 4000
local_int = 5000
local_float = 6000
local_char = 7000
local_bool = 8000
const_int = 9000
const_float = 10000
const_char = 11000
const_bool = 12000
const_string = 13000

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
    'BOOL',         #bool
    'CTE_INT',      #Cte.int
    'CTE_FLOAT',    #Cte.float
    'CTE_CHAR',     #Cte.char
    'CTE_STRING',   #Cte.String
    'CTE_BOOL',     #Cte.bool
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
t_DIF = r'\!\='         #!=
t_SAME = r'\=\='        #==

t_ignore = ' \t'

#Definition of complex tokens
def t_CTE_CHAR(t):
    r"'.'"
    t.value = str(t.value)
    return t

def t_CTE_STRING(t):
    r'\"(.*?)\"'
    t.value = str(t.value)
    return t

def t_PROGRAM(t):
    r'program\b'
    t.type = 'PROGRAM'
    return t

def t_CLASS(t):
    r'class\b'
    t.type = 'CLASS'
    return t 

def t_INHERIT(t):
    r'inherit\b'
    t.type = 'INHERIT'
    return t 

def t_ATTRIBUTES(t):
    r'attributes\b'
    t.type = 'ATTRIBUTES'
    return t 

def t_METHODS(t):
    r'methods\b'
    t.type = 'METHODS'
    return t 

def t_VARS(t):
    r'vars\b'
    t.type = 'VARS'
    return t 

def t_FUNCTION(t):
    r'function\b'
    t.type = 'FUNCTION'
    return t

def t_MAIN(t):
    r'main\b'
    t.type = 'MAIN'
    return t

def t_INT(t):
    r'int\b'
    t.type = 'INT'
    return t

def t_FLOAT(t):
    r'float\b'
    t.type = 'FLOAT'
    return t

def t_CHAR(t):
    r'char\b'
    t.type = 'CHAR'
    return t

def t_BOOL(t):
    r'bool\b'
    t.type = 'BOOL'
    return t

def t_READ(t):
    r'read\b'
    t.type = 'READ'
    return t 

def t_WRITE(t):
    r'write\b'
    t.type = 'WRITE'
    return t 

def t_RETURN(t):
    r'return\b'
    t.type = 'RETURN'
    return t 

def t_VOID(t):
    r'void\b'
    t.type = 'VOID'
    return t 

def t_WHILE(t):
    r'while\b'
    t.type = 'WHILE'
    return t 

def t_DO(t):
    r'do\b'
    t.type = 'DO'
    return t 

def t_FROM(t):
    r'from\b'
    t.type = 'FROM'
    return t

def t_TO(t):
    r'to\b'
    t.type = 'TO'
    return t

def t_IF(t):
    r'if\b'
    t.type = 'IF'
    return t

def t_THEN(t):
    r'then\b'
    t.type = 'THEN'
    return t

def t_ELSE(t):
    r'else\b'
    t.type = 'ELSE'
    return t 

def t_VAR(t):
    r'[a-z][a-zA-Z_0-9]*'
    t.type = 'VAR'
    return t

def t_CTE_BOOL(t):
    r'(True|False)'
    t.type = 'CTE_BOOL'
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

# ***** Operator Table *****
tabOp = {
    'GOTO' : 1,
    'GOTOF' : 2,
    'GOTOV' : 3, 
    '=' : 4,
    '+=' : 5,
    '-=' : 6,
    '*=' : 7,
    '/=' : 8,
    '+' : 9,
    '-' : 10,
    '*' : 11,
    '/' : 12,
    'write' : 13,
    'read' : 14,
    '>' : 15,
    '<' : 16,
    '>=' : 17,
    '<=' : 18,
    '==' : 19,
    '!=' : 20,
    '&' : 21,
    '|' : 22,
    'VER' : 23,
    'ERA' : 24,
    'PARAMETER' : 25,
    'GOSUB' : 26,
    'ENDFUNC' : 27,
    'return' : 28,
    'END' : 29
 }

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
            '|': 'error',
            '=': 'int'
        },
        'float':{
            '-': 'float',
            '*': 'float',
            '/': 'float',
            '-=': 'error',
            '*=': 'error',
            '/=': 'error',
            '+': 'float',
            '+=': 'error',
            '<': 'bool',
            '>': 'bool',
            '<=': 'bool',
            '>=': 'bool',
            '!=': 'bool',
            '==': 'bool',
            '&': 'error',
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'float'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'char'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'error',
            '=': 'error'
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
            '|': 'bool',
            '=': 'bool'
        }
    }
}

# ***** PARSER *****

# Grammar definition
def p_program(p):
    '''
    program : PROGRAM np_startProg ID np_addFunc SEMICOLON programT
    '''
    p[0] = None

def p_programT(p):
    '''
    programT : class programT
             | vars programF
             | programF
    
    programF : func programF 
             | main np_endProg empty
    '''
    p[0] = None

def p_class(p):
    '''
    class : CLASS ID np_addFunc LESS INHERIT ID np_inherit GREATER classT
            | CLASS ID np_addFunc classT
    
    classT : SEMICOLON L_CURPAR ATTRIBUTES dec classTT
            | SEMICOLON L_CURPAR classTT

    classTT : METHODS func classTTT
            | classF

    classTTT : func classTTT
            | classF
        
    classF : R_CURPAR SEMICOLON np_endClass empty
    '''
    p[0] = None

def p_vars(p):
    '''
    vars  : VARS dec empty
    '''
    p[0] = None

def p_dec(p):
    '''
    dec : VAR arr np_getDecArr decF  
        | VAR np_getDec decF 
    '''
    if len(p) == 5:
      p[0] = p[1]+p[2]
    else:
      p[0] = p[1]

def p_decF(p):
    ''' 
    decF : COMMA dec 
          | COLON type np_getVarType SEMICOLON np_getDec dec 
          | COLON type np_getVarType SEMICOLON np_getDec np_addToDic empty
    '''
    if p[1] == ",":
      p[0] = p[2]

def p_type(p):
    '''
    type  : INT empty
          | FLOAT empty
          | CHAR empty
          | BOOL empty
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
    func : typeFunc FUNCTION ID np_addFunc L_PAR funcT

    funcT : parameter R_PAR SEMICOLON funcF
           | R_PAR SEMICOLON funcF

    funcF : dec L_CURPAR statement R_CURPAR np_endFunc empty
           | L_CURPAR statement R_CURPAR np_endFunc empty
    '''

def p_typeFunc(p):
    '''
    typeFunc  : INT empty
              | FLOAT empty
              | CHAR empty
              | BOOL empty
              | VOID empty
    '''
    p[0] = p[1]


def p_paramater(p):
    '''
    parameter : VAR np_getDec COLON typepar np_getVarType np_addParam SEMICOLON np_getDec np_addToDic parameterF
    
    parameterF : parameter
                 | empty
    '''
    p[0] = None

def p_typepar(p):
    '''
    typepar  : INT empty
          | FLOAT empty
          | CHAR empty
          | BOOL empty
    '''
    p[0] = p[1]

def p_main(p):
    '''
    main : MAIN np_getMainFunc L_PAR R_PAR L_CURPAR statement R_CURPAR empty
    '''
    p[0] = None

def p_statement(p):
    '''
    statement : assigment SEMICOLON statementF
                | void np_checkVoidState SEMICOLON statementF
                | return SEMICOLON statementF
                | read SEMICOLON statementF
                | write SEMICOLON statementF
                | if statementF
                | repeat statementF

    statementF : statement
                | empty
    '''
    p[0] = None

def p_void(p):
    '''
    void : VAR DOT ID np_checkVoidClass L_PAR np_eraQuad voidT
            | ID np_checkVoid L_PAR np_eraQuad voidT

    voidT : exp np_checkParam COMMA voidT
            | exp np_checkParam voidF
            | voidF

    voidF : R_PAR np_endVoid empty    
    '''
    p[0] = None

def p_arrfunc(p):
    '''
    arrfunc : L_BREAK np_startArr exp np_ftwoDimArr COMMA exp np_ltwoDimArr R_BREAK np_endArr empty
            | L_BREAK np_startArr exp np_oneDimArr R_BREAK np_endArr empty
    '''
    p[0] = None

def p_return(p):
    '''
    return : RETURN L_PAR exp np_addReturn R_PAR empty
    '''
    p[0] = None

def p_var(p):
    '''
    var : VAR np_addId varF  
        | VAR DOT VAR np_addClassId varF  
    '''

def p_varF(p):
    '''
    varF : arrfunc empty
          | empty
    '''

def p_read(p):
    '''
    read : READ L_PAR readF
    
    readF : var np_addRead COMMA readF 
          | var np_addRead R_PAR empty
    '''
    p[0] = None

def p_write(p):
    '''
    write  : WRITE L_PAR writeT

    writeT : CTE_STRING np_addConstString writeF
            | exp writeF

    writeF : COMMA np_addWrite  writeT
               | R_PAR np_addWrite empty
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
    if : IF L_PAR exp np_checkBool R_PAR THEN L_CURPAR statement R_CURPAR ifF

    ifF : ELSE np_else L_CURPAR statement R_CURPAR np_endIf empty
        | np_endIf empty
    '''
    p[0] = None

def p_assigment(p):
    '''
    assigment : var assigmentF

    assigmentF : EQUAL np_addOp exp np_doAssign empty 
                | ope np_addOp exp np_doAssign empty
    '''
    p[0] = None

def p_ope(p):
    '''
    ope : PLUS_EQ empty
        | MIN_EQ empty
        | MULT_EQ empty
        | DIV_EQ empty
    '''
    p[0] = p[1]

def p_conditional(p):
    '''
    conditional : WHILE np_addWhile L_PAR exp R_PAR np_checkBool DO L_CURPAR statement R_CURPAR np_endWhile empty
    '''
    p[0] = None

def p_nonconditional(p):
    '''
    nonconditional : FROM var nonconditionalF

    nonconditionalF :  EQUAL np_addOp exp np_assingFor TO exp np_checkExp DO L_CURPAR statement R_CURPAR np_endFor empty
    '''
    p[0] = None

def p_bool(p):
    '''
    bool : logical np_addBool boolF

    boolF : OR np_addOp bool
        | AND np_addOp bool
        | empty
    '''
    p[0] = None

def p_logical(p):
    '''
    logical : ex np_addLogical logicalF

    logicalF : LESS np_addOp logical
            | GREATER np_addOp logical
            | LESS_TH np_addOp logical
            | GREAT_TH np_addOp logical
            | SAME np_addOp logical
            | DIF np_addOp logical
            | empty
    '''
    p[0] = None

def p_exp(p):
    '''
    exp : bool empty

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
            | void np_checkVoidExp empty
            | MINUS np_doNegative varcte empty
            | varcte empty
    '''
    p[0] = None

def p_varcte(p):
    '''
    varcte  : CTE_INT np_addConstInt empty
            | CTE_FLOAT np_addConstFloat empty
            | CTE_CHAR np_addConstChar empty
            | CTE_BOOL np_addConstBool empty
            | var empty
    '''
    p[0] = None


#Error function
def p_error(p):
    print("Syntax error found at line %d." % (lexer.lineno))
    print(p)
    sys.exit()


#Empty function
def p_empty(p):
    '''
    empty : 
    '''
    p[0] = None

# ***** NEURALGIC POINTS *****

#Neuralgic point to mark the start of the program
def p_np_startProg(p):
    'np_startProg : '
    quadruples.append(Quadruple(tabOp["GOTO"], -1, -1, 0))
    quadaux.append(Quadruple("GOTO", None, None, 0))
    pjumps.append(len(quadruples) - 1)

#Neuralgic point to get program name when main start and mark where the program start
def p_np_getMainFunc(p):
    'np_getMainFunc : '
    global currFunc, progName
    currFunc = progName
    jump = pjumps.pop()
    quadruples[jump].temp = len(quadruples)
    quadaux[jump].temp = len(quadruples)

#Neuralgic point to add function to process dictionary
def p_np_addFunc(p):
    'np_addFunc : '
    global currFunc, progName, contReturns, currType, auxiliarDic, dic
    currFunc = p[-1]
    if p[-3] == 'program':
        dic.addFunc(currFunc, "program")
        progName = currFunc
    elif p[-2] == 'class':
        checkClass(currFunc)
        auxiliarDic = copy(dic)
        dic.clearDic()
        dic.addFunc(currFunc, "class")
        aux = progName
        pcalls.append(aux)
        progName = currFunc
    else:
        if dic.funcOccupied(currFunc):
            error('Function "{}" has already been declared'.format(currFunc))
        else:
            dic.addFunc(currFunc, p[-3], len(quadruples))
            if p[-3] != 'void':
                currType = p[-3]
                pvars.append(currFunc)
                addVars(progName)
                dic.setMemAd(currFunc, dic.getVarMemo(progName, currFunc))
                contReturns = 0

#Neuralgic point to inherit a class
def p_np_inherit(p):
    'np_inherit : '
    global arrClases, dic, global_int, global_float, global_char, global_bool
    father = p[-1]
    son = p[-5]
    checkClassExist(father)
    for i in arrClases:
        if father == i[0]:
            dic.getCopy(i[1], son, father)
    global_int = dic.dic[son].memory[0] + global_int
    global_float = dic.dic[son].memory[1] + global_float 
    global_char = dic.dic[son].memory[2] + global_char
    global_bool = dic.dic[son].memory[3] + global_bool
    
#Neuralgic point to add variable to vars stack
def p_np_getDec(p):
    'np_getDec : '
    pvars.append(p[-1])

#Neuralgic point to add variable with array to vars stack
def p_np_getDecArr(p):
    'np_getDecArr : '
    pvars.append(p[-2]+p[-1])

#Neuralgic point to add variable type to vars type stack
def p_np_getVarType(p):
    'np_getVarType : '
    global currFunc, progName
    if match(r'[A-Z][a-zA-Z0-9]*', p[-1]):
        if currFunc != progName:
            error("Can't operate objects inside functions")
        getDic(p[-1])
    pvarsT.append(p[-1])

#Neuralgic point to add parameter in process table and process dictionary
def p_np_addParam(p):
    'np_addParam : '
    global currFunc
    dic.addParam(currFunc, p[-2])

#Neuralgic point to start adding variable to process table
def p_np_addToDic(p):
    'np_addToDic : '
    global currFunc, progName
    while pvars:
        addVars(currFunc)

'''
Function that add variable to process table

Parameters
----------
key : str -> Name of a function

Returns
----------
'''
def addVars(key):
    global currType
    item = pvars[-1]
    if item == ";":
        currType = pvarsT.pop()
        pvars.pop()
    elif item[-1] == "]":
        lvl1 = 1
        lvl2 = 1
        v = item.split("[")
        if "," in v[1]:
            a = v[1].split(",")
            lvl1 = int(a[0])
            lvl2 = int(a[1].replace(']', ''))
        else:
            lvl1 = int(v[1].replace(']', ''))
        if dic.varOccupied(key,v[0]):
            error('Variable "{}" has already been declared'.format(v[0]))
        else:
            if match(r'[A-Z][a-zA-Z0-9]*', currType):
                error("Can't handle lists of objects")
            memo = getMemo(key, lvl1*lvl2)
            dic.addVar(key, v[0], currType, memo, lvl1, lvl2)
            pvars.pop()

    else:
        if dic.varOccupied(key,item):
            error('Variable "{}" has already been declared'.format(item))
        else:
            memo = getMemo(key)
            dic.addVar(key, item, currType, memo)
            pvars.pop()

'''
Function that returns the memory address of the function

Parameters
----------
key : str -> Nome of  afunction
memChunck : int -> Number of address in a memory chunk (default : 1)

Returns
----------
inr -> memory address
'''
def getMemo(key, memChunck = 1):
    global currType, progName, global_int, global_float, global_char, global_bool, local_int, local_float, local_char, local_bool, global_class, local_class
    memo = ""
    if key == progName:
        if currType == 'int':
            if global_int > 1999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_int
            global_int += memChunck
        elif currType == 'float':
            if global_float > 2999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_float
            global_float += memChunck
        elif currType == 'char':
            if global_char > 3999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_char
            global_char += memChunck
        elif currType == 'bool':
            if global_bool > 4999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_bool
            global_bool += memChunck
        elif match(r'[A-Z][a-zA-Z0-9]*', currType):
            if global_class > 499: 
                error('Limit of classes reached'.format(currType))
            if dic.getFuncType(progName) == 'class':
                error("Can't operate objects inside clases")
            memo = global_class
            global_class += memChunck
    else: 
        if currType == 'int':
            if local_int > 5999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_int
            local_int += memChunck
        elif currType == 'float':
            if local_float > 6999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_float
            local_float += memChunck
        elif currType == 'char':
            if local_char > 7999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_char
            local_char += memChunck
        elif currType == 'bool':
            if local_bool > 8999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_bool
            local_bool += memChunck
        elif match(r'[A-Z][a-zA-Z0-9]*', currType):
            if local_class > 999: 
                error('Limit of classes reached'.format(currType))
            memo = local_class
            local_class += memChunck
    return memo

'''
Function that return memory number of a constant

Parameters
----------
vart : str -> Type of the constant

Returns
----------
inr -> memory address
'''
def get_const_memo(vart):
    global const_int, const_float, const_char, const_bool, const_string 
    if vart == 'int':
        if const_int > 9999: 
            error('Limit of variables of type {} reached'.format(vart))
        memo = const_int
        const_int += 1
    elif vart == 'float':
        if const_float > 10999: 
            error('Limit of variables of type {} reached'.format(vart))
        memo = const_float
        const_float += 1
    elif vart == 'char':
        if const_char > 11999: 
            error('Limit of variables of type {} reached'.format(vart))
        memo = const_char
        const_char += 1
    elif vart == 'bool':
        if const_bool > 12999: 
            error('Limit of variables of type {} reached'.format(vart))
        memo = const_bool
        const_bool += 1
    elif vart == 'string':
        if const_string > 13999: 
            error('Limit of variables of type {} reached'.format(vart))
        memo = const_string
        const_string += 1
    return memo

#Neuralgic point to process the end of a function
def p_np_endFunc(p):
    'np_endFunc : '
    global currFunc, local_int, local_float, local_char, local_bool, contReturns, local_class
    if dic.getFuncType(currFunc) != 'void' and  dic.getFuncType(currFunc) != 'program' and contReturns < 1:
        error("Function {} expect a return value".format(currFunc))
    quadruples.append(Quadruple(tabOp['ENDFUNC'], -1, -1, -1))
    quadaux.append(Quadruple("ENDFUNC", None, None, None))
    dic.dic[currFunc].memory[0] = local_int - 5000
    dic.dic[currFunc].memory[1] = local_float - 6000
    dic.dic[currFunc].memory[2] = local_char - 7000
    dic.dic[currFunc].memory[3] = local_bool - 8000
    if local_class > 500:
        arraux = []
        for i in range(local_class-500):
            arraux.append((i, dic.getTypeFromMemo(currFunc, i)))
        dic.dic[currFunc].memory[4] = arraux
    local_int = 5000
    local_float = 6000
    local_char = 7000
    local_bool = 8000
    local_class = 500
    #dic.printFunc(currFunc)
    dic.delVar(currFunc)

#Neuralgic point to add id in operand stack
def p_np_addId(p):
    'np_addId : '
    check_type_id(p[-1])
    pilaO.append(p[-1])

#Neuralgic point to add a class id in operand stack
def p_np_addClassId(p):
    'np_addClassId : '
    global currFunc
    varCall = p[-3]
    var = p[-1]
    if not dic.varOccupied(currFunc, varCall):
        error("Object {} not defined".format(varCall))
    cName = dic.getVarType(currFunc, varCall)
    auxDic = getDic(cName, varCall)
    if auxDic.varOccupied(cName,var):
        ptypes.append(auxDic.getVarType(cName, var))
    else: 
        error('Variable "{}" not defined on Class {}'.format(var, cName))

    memaux = auxDic.getVarMemo(cName, var)
    pilaO.append(str(dic.getVarMemo(currFunc, varCall)) + '.' + str(memaux))


#Neuralgic point that add constant int in operand stack
def p_np_addConstInt(p):
    'np_addConstInt : '
    global const_int
    if p[-1] not in const_table:
        aux = get_const_memo('int')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'int'
        }
    pilaO.append(const_table[p[-1]]['memo'])
    ptypes.append('int')

#Neuralgic point that add constant float in operand stack
def p_np_addConstFloat(p):
    'np_addConstFloat : '
    global const_float
    if p[-1] not in const_table:
        aux = get_const_memo('float')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'float'
        }
    pilaO.append(const_table[p[-1]]['memo'])
    ptypes.append('float')

#Neuralgic point that add constant char in operand stack
def p_np_addConstChar(p):
    'np_addConstChar : '
    global const_char
    char = p[-1]
    char = char[1:-1]
    if char not in const_table:
        aux = get_const_memo('char')
        const_table[char] = {
            'memo': aux,
            'type': 'char'
        }
    pilaO.append(const_table[char]['memo'])
    ptypes.append('char')

#Neuralgic point that add constant bool in operand stack
def p_np_addConstBool(p):
    'np_addConstBool : '
    global const_bool
    if p[-1] not in const_table:
        aux = get_const_memo('bool')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'bool'
        }
    pilaO.append(const_table[p[-1]]['memo'])
    ptypes.append('bool')

#Neuralgic point that add constant string in operand stack
def p_np_addConstString(p):
    'np_addConstString : '
    global const_tring
    string = p[-1]
    string = string[1:-1]
    if string not in const_table:
        aux = get_const_memo('string')
        const_table[string] = {
            'memo': aux,
            'type': 'string'
        }
    pilaO.append(const_table[string]['memo'])
    ptypes.append('string')

'''
Function to check type of ids

Parameters
----------
check : str -> Name of the variable
'''
def check_type_id(check):
    global currFunc, arrFunc, progName
    if dic.varOccupied(currFunc,check):
        t = dic.getVarType(currFunc, check)
        if match(r'[A-Z][a-zA-Z0-9]*', t):
            error("Can't operate Object {}".format(check))
        ptypes.append(t)
        if dic.isArr(currFunc, check):
            arrFunc = copy(currFunc)
    elif dic.varOccupied(progName,check):
        ptypes.append(dic.getVarType(progName, check))
        if dic.isArr(progName, check):
            arrFunc = copy(progName)
    else:
        #dic.printAll()
        error('Variable "{}" not defined'.format(check))

#Neuralgic point to add operator in opertator stack
def p_np_addOp(p):
    'np_addOp : '
    poper.append(p[-1])

#Neuralgic point to add parentesis in operator stack
def p_np_addPar(p):
    'np_addPar : '
    poper.append(p[-1])

#Neuralgic point to pop fake bottom in operator stack
def p_np_popPar(p):
    'np_popPar : '
    if poper[-1] != '(':
        error('Error with fake bottom')
    else:
        poper.pop()

#Neuralgic point to start generation quadruple from a term
def p_np_addTerm(p):
    'np_addTerm : '
    generateQuad(['*','/'])

#Neuralgic point to start generation quadruple from a ex
def p_np_addEx(p):
    'np_addEx : '
    generateQuad(['+','-'])

#Neuralgic point to start generation quadruple from a Logical
def p_np_addLogical(p):
    'np_addLogical : '
    generateQuad(['<','>','>=','<=','==','!='])

#Neuralgic point to start generation quadruple from a bool
def p_np_addBool(p):
    'np_addBool : '
    generateQuad(['|','&'])

#Neuralgic point that makes the next variable negative
def p_np_doNegative(p):
    'np_doNegative : '
    if -1 not in const_table:
        aux = get_const_memo('int')
        const_table[-1] = {
            'memo': aux,
            'type': 'int'
        }
    pilaO.append(const_table[-1]['memo'])
    ptypes.append('int')
    poper.append('*')

#Neuralgic point to generate assigment quadruple
def p_np_doAssign(p):
    'np_doAssign : '
    global currFunc
    op = poper.pop()
    opdo_der = pilaO.pop()
    opdoT_der = ptypes.pop()
    temp = pilaO.pop()
    tempT = ptypes.pop()

    if dic.chechArr(currFunc, opdo_der):
        error("Can't operate array {}".format(opdo_der))
    if dic.chechArr(currFunc, temp):
        error("Can't operate array {}".format(temp))

    check = semanticCube[opdoT_der][tempT][op]
    if check != 'error':
        quadaux.append(Quadruple(op, opdo_der, None, temp))
        opdo_der = changeToMem(opdo_der)
        temp = changeToMem(temp)
        quadruples.append(Quadruple(tabOp[op], opdo_der, -1, temp))
    else:
        error('Type {} could not be assign with type {}'.format(tempT, opdoT_der))

#Neuralgic point to start the process of an array
def p_np_startArr(p):
    'np_startArr : '
    global arrFunc
    opdo = pilaO.pop()
    ptypes.pop()
    if dic.isArr(arrFunc, opdo):
        pdim.append((opdo, 1))
        poper.append('(')
    else:
        error("Variable {} have no dimensions".format(opdo))

#Neuralgic point that process a 1 dimensional array
def p_np_oneDimArr(p):
    'np_oneDimArr : '
    global arrFunc
    aux = pdim[-1]
    opdo = aux[0]
    if dic.checkOneDim(arrFunc, opdo):
        temp = pilaO[-1]
        tempT = ptypes.pop()
        lvl = dic.getLvl1(arrFunc, opdo)
        if tempT != 'int':
            error("Indexes can only be type int")
        quadaux.append(Quadruple('VER', temp, 0, lvl))
        temp = changeToMem(temp)
        quadruples.append(Quadruple(tabOp['VER'], temp, 0, lvl))
    else:
        error("Variable {} expect two indexes and received one".format(opdo))

#Neuralgic point that process the first part of a 2 dimensional array
def p_np_ftwoDimArr(p):
    'np_ftwoDimArr : '
    global arrFunc
    aux = pdim.pop()
    opdo = aux[0]
    if dic.checkTwoDim(arrFunc, opdo):
        temp = pilaO.pop()
        tempT = ptypes.pop()
        lvl = dic.getLvl1(arrFunc, opdo)
        if tempT != 'int':
            error("Indexes can only be type int")
        quadaux.append(Quadruple('VER', temp, 0, lvl))
        tempaux = temp
        temp = changeToMem(temp)
        quadruples.append(Quadruple(tabOp['VER'], temp, 0, lvl))

        lvl2 = dic.getLvl2(arrFunc, opdo)

        if lvl2 not in const_table:
            aux = get_const_memo('int')
            const_table[lvl2] = {
                'memo': aux,
                'type': 'int'
            }
        d2 = const_table[lvl2]['memo']
        temp2 = generate_temporal('int')
        quadaux.append(Quadruple('*', tempaux, d2, temp2))
        quadruples.append(Quadruple(tabOp['*'], temp, d2, temp2))
        pilaO.append(temp2)
        pdim.append((opdo,2))
    else:
        error("Variable {} expect one index and received two".format(opdo))

#Neuralgic point that process the first part of a 2 dimensional array
def p_np_ltwoDimArr(p):
    'np_ltwoDimArr : '
    global arrFunc
    aux = pdim[-1]
    opdo = aux[0]
    temp = pilaO.pop()
    tempT = ptypes.pop()
    lv2 = dic.getLvl2(arrFunc, opdo)
    if tempT != 'int':
        error("Indexes can only be type int")
    quadaux.append(Quadruple('VER', temp, 0, lv2))
    tempaux = temp
    temp = changeToMem(temp)
    quadruples.append(Quadruple(tabOp['VER'], temp, 0, lv2))

    temp2 = pilaO.pop()
    temp3 = generate_temporal('int')
    quadaux.append(Quadruple('+', tempaux, temp2, temp3))
    quadruples.append(Quadruple(tabOp['+'], temp, temp2, temp3))
    pilaO.append(temp3)

#Neuralgic point to process the end of an array
def p_np_endArr(p):
    'np_endArr : '
    global arrFunc
    auxdim = pdim.pop()
    id = auxdim[0]
    opdo = pilaO.pop()
    idmem = dic.getVarMemo(arrFunc, id)

    if idmem not in const_table:
        aux = get_const_memo('int')
        const_table[idmem] = {
            'memo': aux,
            'type': 'int'
        }
    virAd = const_table[idmem]['memo']
    temp = generate_temporal('int')
    quadaux.append(Quadruple('+', opdo, virAd, temp))
    opdo = changeToMem(opdo)
    quadruples.append(Quadruple(tabOp['+'], opdo, virAd, temp))
    
    pilaO.append("(" + str(temp) +  ")")
    ptypes.append(dic.getVarType(arrFunc,id))
    poper.pop()

#Neuralgic point to generate read quadruple
def p_np_addRead(p):
    'np_addRead : '
    global currFunc
    temp = pilaO.pop()
    ptypes.pop()

    if dic.chechArr(currFunc, temp):
        error("Can't operate array {}".format(temp))

    quadaux.append(Quadruple('read', None, None, temp))
    temp = changeToMem(temp)
    quadruples.append(Quadruple(tabOp['read'], -1, -1, temp))

#Neuralgic point to generate write quadruple
def p_np_addWrite(p):
    'np_addWrite : '
    global currFunc
    if len(pilaO) > 0:
        ptypes.pop()
        opdo = pilaO.pop()

        if dic.chechArr(currFunc, opdo):
            error("Can't operate array {}".format(opdo))

        quadaux.append(Quadruple('write', None, None, opdo))
        opdo = changeToMem(opdo)
        quadruples.append(Quadruple(tabOp['write'], -1, -1, opdo))

#Neuralgic point to generate return quadruple
def p_np_addReturn(p):
    'np_addReturn : '
    global currFunc, contReturns
    if dic.getFuncType(currFunc) != 'void' and dic.getFuncType(currFunc) != 'program':
        if len(pilaO) > 0:
            opdoT = ptypes.pop()
            opdo = pilaO.pop()

            if dic.chechArr(currFunc, opdo):
                error("Can't operate array {}".format(opdo))
            if opdoT ==  dic.getFuncType(currFunc):
                contReturns += 1
                quadaux.append(Quadruple('return', None, None, opdo))
                opdo = changeToMem(opdo)
                quadruples.append(Quadruple(tabOp['return'], -1, -1, opdo))
            else:
                error("Expected return value type {}".format(dic.getFuncType(currFunc)))
    else:
        error("Function {} can't return any value".format(currFunc))

#Neuralgic point to generate if quadruple
def p_np_checkBool(p):
    'np_checkBool : '
    global currFunc
    check = ptypes.pop()
    if check == 'bool':
        opdo = pilaO.pop()

        if dic.chechArr(currFunc, opdo):
            error("Can't operate array {}".format(opdo))

        quadaux.append(Quadruple('GOTOF', opdo, None, 0))
        opdo = changeToMem(opdo)
        quadruples.append(Quadruple(tabOp['GOTOF'], opdo, -1, 0))
        pjumps.append(len(quadruples) - 1)
    else:
        error('Type mismatch, expected value of type bool')

#Neuralgic point save the end position of the if 
def p_np_endIf(p):
    'np_endIf : '
    jump = pjumps.pop()
    quadruples[jump].temp = len(quadruples)
    quadaux[jump].temp = len(quadruples)

#Neuralgic point to generate GOTO quadruple of else
def p_np_else(p):
    'np_else : '
    quadruples.append(Quadruple(tabOp['GOTO'], -1, -1, 0))
    quadaux.append(Quadruple("GOTO", None, None, 0))
    jump = pjumps.pop()
    quadruples[jump].temp = len(quadruples)
    quadaux[jump].temp = len(quadruples)
    pjumps.append(len(quadruples) - 1)

#Neuralgic point to save start position of while
def p_np_addWhile(p):
    'np_addWhile : '
    pjumps.append(len(quadruples))

#Neuralgic point to add GOTO quadruple of while
def p_np_endWhile(p):
    'np_endWhile : '
    startW = pjumps.pop()
    temp = pjumps.pop()
    quadruples.append(Quadruple(tabOp['GOTO'], -1, -1, temp))
    quadaux.append(Quadruple('GOTO', None, None, temp))
    endW = len(quadruples)
    quadruples[startW].temp = endW
    quadaux[startW].temp = endW

#Neuralgic point to assign vaiable insede a for loop
def p_np_assingFor(p):
    'np_assingFor : '
    global currFunc
    op = poper.pop()
    opdo_der = pilaO.pop()
    opdoT_der = ptypes.pop()
    temp = pilaO.pop()
    tempT = ptypes.pop()

    if dic.chechArr(currFunc, opdo_der):
        error("Can't operate array {}".format(opdo_der))
    if dic.chechArr(currFunc, temp):
        error("Can't operate array {}".format(temp))

    if tempT != 'int' or opdoT_der != 'int':
        error('Expected type int')
    else:
        quadaux.append(Quadruple(op, opdo_der, None, temp))
        opdo_der = changeToMem(opdo_der)
        temp = changeToMem(temp)
        quadruples.append(Quadruple(tabOp[op], opdo_der, -1, temp))
        ptypes.append(tempT)
        pilaO.append(temp)
        pilaO.append(temp)

#Neuralgic point to check resulting expresion and genetra GOTOV quadruple
def p_np_checkExp(p):
    'np_checkExp : '
    global currFunc
    poper.append('>')
    generateQuad(['>'])
    pjumps.append(len(quadruples) - 1)
    check = ptypes.pop()
    if check == 'bool':
        opdo = pilaO.pop()

        if dic.chechArr(currFunc, opdo):
            error("Can't operate array {}".format(opdo))

        quadaux.append(Quadruple('GOTOV', opdo, None, 0))
        opdo = changeToMem(opdo)
        quadruples.append(Quadruple(tabOp['GOTOV'], opdo, -1, 0))
        pjumps.append(len(quadruples) - 1)
    else:
        error('Type mismatch, expected value of type bool')

#Neuralgic point to process the end of a for loop 
def p_np_endFor(p):
    'np_endFor : '
    global currFunc
    currVar = pilaO.pop()
    if 1 not in const_table:
        aux = get_const_memo('int')
        const_table[1] = {
            'memo': aux,
            'type': 'int'
        }
    const = (const_table[1]['memo'])
    quadaux.append(Quadruple('+=', const, None, currVar))
    currVar = changeToMem(currVar)
    quadruples.append(Quadruple(tabOp['+='], const, -1, currVar))
    startW = pjumps.pop()
    temp = pjumps.pop()
    quadruples.append(Quadruple(tabOp['GOTO'], -1, -1, temp))
    quadaux.append(Quadruple('GOTO', None, None, temp))
    endW = len(quadruples)
    quadruples[startW].temp = endW
    quadaux[startW].temp = endW

#Neuralgic point to verify if a function exist in a function call
def p_np_checkVoid(p):
    'np_checkVoid : '
    global progName
    if dic.funcOccupied(p[-1]) and p[-1] != progName:
        pcalls.append(p[-1])
        poper.append('(')

    else:
        error("Function {} is not declared".format(p[-1]))

#Neuralgic point to verify if a function exist in a function call on a class
def p_np_checkVoidClass(p):
    'np_checkVoidClass : '
    global currFunc
    vcall = p[-3]
    cName = dic.getVarType(currFunc, vcall)
    memaux = dic.getVarMemo(currFunc, vcall)
    fName = p[-1]
    auxDic = getDic(cName, vcall)
    if auxDic.funcOccupied(fName) and fName != cName:
        pcalls.append(str(memaux) + '.' + str(cName) + '.' + str(fName))
        poper.append('(')
        pClassCalls.append(cName)
    else:
        error("Function {} is not declared on Class {}".format(fName, cName))


#Neuralgic point that check that the function call returns a value
def p_np_checkVoidExp(p):
    'np_checkVoidExp : '
    currCall = pcalls.pop()
    if pClassCalls:
        curCallaux = currCall.split('.')
        auxDic = getDic(pClassCalls[-1])
        pClassCalls.pop()
        if auxDic.getFuncType(curCallaux[2]) == 'void':
            error("Function {} does not return any value".format(curCallaux[2]))
    else:
        auxDic = copy(dic)
        if auxDic.getFuncType(currCall) == 'void':
            error("Function {} does not return any value".format(currCall))


#Neuralgic point that check that the function call doen not return a value
def p_np_checkVoidState(p):
    'np_checkVoidState : '
    currCall = pcalls.pop()
    if pClassCalls:
        curCallaux = currCall.split('.')
        auxDic = getDic(pClassCalls[-1])
        pClassCalls.pop()
        if auxDic.getFuncType(curCallaux[2]) != 'void':
            error("Function {} should be void".format(curCallaux[2]))
    else:
        auxDic = copy(dic)
        if auxDic.getFuncType(currCall) != 'void':
            error("Function {} should be void".format(currCall))

    
    

#Neuralgic point to create ERA cuadruple
def p_np_eraQuad(p):
    'np_eraQuad : '
    global paramK
    currCall = pcalls[-1]
    quadruples.append(Quadruple(tabOp['ERA'], currCall, -1, -1))
    quadaux.append(Quadruple("ERA", currCall, None, None))
    if paramK > 0:
        pParams.append(paramK)
    paramK = 0

#Neuralgi point to process parameters on function calls
def p_np_checkParam(p):
    'np_checkParam : '
    global paramK, currFunc
    currCall = pcalls[-1]
    opdo = pilaO.pop()

    if pClassCalls:
        auxDic = getDic(pClassCalls[-1])
        curCallaux = currCall.split('.')
        currCall = curCallaux[2]
    else:
        auxDic = copy(dic)

    if dic.chechArr(currFunc, opdo):
        error("Can't operate array {}".format(opdo))

    opdoT = ptypes.pop()
    paramK += 1
    if paramK <= auxDic.funcParamSize(currCall):
        if opdoT == auxDic.funcParam(currCall, paramK):
            quadaux.append(Quadruple('PARAMETER', opdo, None, 'par' + str(paramK)))
            opdo = changeToMem(opdo)
            quadruples.append(Quadruple(tabOp['PARAMETER'], opdo, -1, paramK))
        else: 
            error("Expected type {} on call to function {}".format(auxDic.funcParam(currCall, paramK), currCall))

#Neuralgic point that mark the end of a function call
def p_np_endVoid(p):
    'np_endVoid : '
    global paramK, progName
    currCall = pcalls[-1]

    if pClassCalls:
        auxDic = getDic(pClassCalls[-1])
        pName = pClassCalls[-1]
        curCallaux = currCall.split('.')
        currFunc = curCallaux[2]
    else:
        auxDic = copy(dic)
        pName = progName
        currFunc = currCall

    if paramK > auxDic.funcParamSize(currFunc):
        error("{} takes {} parameters but {} were given".format(currFunc, auxDic.funcParamSize(currFunc), paramK))
    elif paramK < auxDic.funcParamSize(currFunc):
        error("{} mising {} parameters".format(currFunc, auxDic.funcParamSize(currFunc) - paramK))
    else:
        poper.pop()
        quadruples.append(Quadruple(tabOp['GOSUB'], currCall, -1, -1))
        quadaux.append(Quadruple("GOSUB", currCall, None, None))
        if auxDic.getFuncType(currFunc) != 'void' and auxDic.getFuncType(currFunc) != 'program':
            tempType = auxDic.getFuncType(currFunc)
            temp = generate_temporal(tempType)
            if "curCallaux" in locals():
                quadaux.append(Quadruple('=', curCallaux[0] + '.' + currFunc, None, temp))
                quadruples.append(Quadruple(tabOp["="], curCallaux[0] + '.' + str(auxDic.getVarMemo(pName, currFunc)), -1, temp))
            else:
                quadaux.append(Quadruple('=', currFunc, None, temp))
                quadruples.append(Quadruple(tabOp["="], auxDic.getVarMemo(pName, currFunc), -1, temp))
            pilaO.append(temp)
            ptypes.append(tempType)
        if pParams:
            paramK = pParams.pop()
        else:
            paramK = 0

'''
Generate logical and aritmetical quadruples

Parameters
----------
check : str -> Operator
'''
def generateQuad(check):
    global currFunc
    if len(poper) > 0:
        if poper[-1] in check:
            op = poper.pop()
            opdo_der = pilaO.pop()
            opdoT_der = ptypes.pop()
            opdo_izq = pilaO.pop()
            opdoT_izq = ptypes.pop()

            if dic.chechArr(currFunc, opdo_der):
                error("Can't operate array {}".format(opdo_der))
            if dic.chechArr(currFunc, opdo_izq):
                error("Can't operate array {}".format(opdo_izq))

            tempType = semanticCube[opdoT_der][opdoT_izq][op]
            if tempType == 'error':
                error('Invalid operation')
            else:
                temp = generate_temporal(tempType)
                pilaO.append(temp)
                ptypes.append(tempType)
                quadaux.append(Quadruple(op, opdo_izq, opdo_der, temp))
                opdo_izq = changeToMem(opdo_izq)
                opdo_der = changeToMem(opdo_der)
                quadruples.append(Quadruple(tabOp[op], opdo_izq, opdo_der, temp))

'''
Generate temporal memory addres

Parameters
----------
tempType : str -> Temporal type

Returns
----------
int : Memory addres of the temporal
'''
def generate_temporal(tempType):
    global currFunc, progName, global_int, global_float, global_char, global_bool, local_int, local_float, local_char, local_bool
    temp = ""
    if currFunc == progName:
        if tempType == 'int':
            if global_int > 1999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = global_int
            global_int += 1
        elif tempType == 'float':
            if global_float > 2999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = global_float
            global_float += 1
        elif tempType == 'char':
            if global_char > 3999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = global_char
            global_char += 1
        elif tempType == 'bool':
            if global_bool > 4999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = global_bool
            global_bool += 1
    else:
        if tempType == 'int':
            if local_int > 5999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = local_int
            local_int += 1
        elif tempType == 'float':
            if local_float > 6999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = local_float
            local_float += 1
        elif tempType == 'char':
            if local_char > 7999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = local_char
            local_char += 1
        elif tempType == 'bool':
            if local_bool > 8999: 
                error('Limit of variables of type {} reached'.format(tempType))
            temp = local_bool
            local_bool += 1
    return temp
        
#Neuralgic point to mark the end of the program
def p_np_endProg(p):
    'np_endProg : '
    global progName, global_int, global_float, global_char, global_bool, global_class
    dic.dic[progName].memory[0] = global_int - 1000
    dic.dic[progName].memory[1] = global_float - 2000
    dic.dic[progName].memory[2] = global_char - 3000
    dic.dic[progName].memory[3] = global_bool - 4000
    if global_class > 0:
        arraux = []
        for i in range(global_class):
            arraux.append((i, dic.getTypeFromMemo(progName, i)))
        dic.dic[progName].memory[4] = arraux
        
    dic.delVar(progName)
    quadaux.append(Quadruple('END', None, None, None))
    quadruples.append(Quadruple(tabOp['END'], -1, -1, -1))

#Neuralgic point that marks the end of a class
def p_np_endClass(p):
    'np_endClass : '
    global global_int, global_float, global_char, global_bool, arrClases, currFunc, progName, dic, auxiliarDic
    dic.dic[progName].memory[0] = global_int - 1000
    dic.dic[progName].memory[1] = global_float - 2000
    dic.dic[progName].memory[2] = global_char - 3000
    dic.dic[progName].memory[3] = global_bool - 4000
    dicAux = copy(dic)
    arrClases.append((progName, dicAux))
    dic.clearDic()
    dic = copy(auxiliarDic)
    global_int = 1000
    global_float = 2000
    global_char = 3000
    global_bool = 4000
    progName =  pcalls.pop()
    currFunc = progName

'''
Change a variable name into its memory address

Parameters
----------
var : str -> variable name

Returns
----------
int : Memory addres of the variable
'''
def changeToMem(var):
    global currFunc, progName
    if type(var) != int and not match(r'\d+\.\d+', var):
        if dic.varOccupied(currFunc,var):
            var = dic.getVarMemo(currFunc, var)
        elif dic.varOccupied(progName,var):
            var = dic.getVarMemo(progName, var)
    return var

'''
Get the dictionary a specific Class

Parameters
----------
c : str -> Class Name
var : str -> Tells if its gonna check for an object or a class (default : N)
'''
def getDic(c, var = "N"):
    for i in arrClases:
        if c == i[0]:
            return i[1]
    if var == 'N':
        error("Class {} not defined".format(c))
    else:
        error("Object {} not defined".format(var))

'''
Checks if a class has alredy been declared

Parameters
----------
c : str -> Class Name
'''
def checkClass(c):
    for i in arrClases:
        if c == i[0]:
            error("Class {} has already been declared".format(c))

'''
Checks if a class exist

Parameters
----------
c : str -> Class Name
'''
def checkClassExist(c):
    cont = 0
    for i in arrClases:
        if c == i[0]:
            cont += 1
    if cont == 0:        
        error("Class {} does not exist".format(c))

'''
Function to display errors

Parameters
----------
line : str -> Explanation of the error
'''
def error(line):
    print("Line " + str(lexer.lineno) + ": " + line)
    sys.exit()

parser = yacc.yacc()

'''
Prints the Operator, Operand, jumps and types stacks, also prints all the quadrupels
'''
def printAll():
    print(pilaO) 
    print(poper)
    print(ptypes)
    print(pjumps)
    #To check quaruples withou memory addresses, use quadaux instead of quadruples
    for index, item in enumerate(quadruples):
        print(index, item.get_quad())

'''
Recieves a .patrol file and compile it 
'''
def main():
    text = input('Insert your program file (.patrol): ')
    if ".patrol" in text : 
        try:
            with open(text, 'r') as file:
                parser.parse(file.read())
        except EOFError:
            sys.exit("Error: File doesn't exist")
        # dic.printAll()
        # for i  in arrClases:
        #     i[1].printAll()
        # printAll()
    else:
        sys.exit("Error: File isn't a Pau Patrol++ program")

'''    
Fucntion to help the virtual machine to get all it need to process the code

Returns
--------
dictionary : Dictionary with the main structures from compilation
'''
def vmHelper():
    main()
    out = {
        'dictionary': dic,
        'const_table': const_table,
        'quadruples': quadruples,
        'arrClases': arrClases
    }
    return out

if __name__ == "__main__":
    main()
