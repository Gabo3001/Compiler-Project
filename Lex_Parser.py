# LEX_Parser.py by Gabriel Ortega and Paulina Cámara (2021)
# Lexer and Parser program using ply

import ply.lex as lex
import ply.yacc as yacc
import sys
from datastruct import DirProcess
from collections import deque
from ObjQuad import Quadruple

dic = DirProcess()

pvars = deque()
pvarsT = deque()
currFunc = ''
currType = ''
progName = ''
currVar = ''

pilaO = deque()
poper = deque()
ptypes = deque()
pjumps = deque()

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

const_table = {}

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
    r"'[a-zA-Z0-9!@#$%^&*()]'"
    t.value = str(t.value)
    return t

def t_CTE_STRING(t):
    r'\"[\w\d\s\,. ]*\"'
    t.value = str(t.value)
    return t

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

def t_BOOL(t):
    r'bool'
    t.type = 'BOOL'
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
    program : PROGRAM ID np_addFunc SEMICOLON programT
    '''
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
    func : typeFunc FUNCTION ID np_addFunc L_PAR funcF
    '''
    p[0] = None

def p_funcF(p):
    '''
    funcF : parameter R_PAR SEMICOLON dec L_CURPAR statement R_CURPAR empty
           | R_PAR SEMICOLON dec L_CURPAR statement R_CURPAR empty
    '''

def p_typeFunc(p):
    '''
    typeFunc  : INT empty
              | FLOAT empty
              | CHAR empty
              | BOOL empty
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
          | BOOL empty
          | ID empty
    '''
    p[0] = None

def p_main(p):
    '''
    main : MAIN np_getMainFunc L_PAR R_PAR L_CURPAR statement R_CURPAR empty
    '''
    p[0] = None

def p_statement(p):
    '''
    statement : assigment SEMICOLON statementF
                | void SEMICOLON statementF
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
    void : ID  DOT ID L_PAR param R_PAR empty
            | ID L_PAR param R_PAR empty
            | ID L_PAR R_PAR empty
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
    return : RETURN L_PAR exp np_addReturn R_PAR empty
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
    assigment : var np_addId assigmentF

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
    nonconditional : FROM VAR arrfunc nonconditionalF
                    | FROM VAR np_addId nonconditionalF 

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
            | CTE_INT np_addConstInt empty
            | CTE_FLOAT np_addConstFloat empty
            | CTE_CHAR np_addConstChar empty
            | CTE_BOOL np_addConstBool empty
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

#Neuralgic point to get program name when main start
def p_np_getMainFunc(p):
  'np_getMainFunc : '
  global currFunc, progName
  currFunc = progName

#Neuralgic point to add function to process dictionary
def p_np_addFunc(p):
  'np_addFunc : '
  global currFunc, progName
  currFunc = p[-1]
  if p[-2] == 'program':
      dic.addFunc(currFunc, "program")
      dic.funcPrint(currFunc)
      progName = currFunc
  else:
      dic.addFunc(currFunc, p[-3])
      dic.funcPrint(currFunc)

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
    pvarsT.append(p[-1])

#Neuralgic point to start adding variable to process table
def p_np_addToDic(p):
    'np_addToDic : '
    global currFunc, progName
    if progName == currFunc:
        while pvars:
            addVars(currFunc)
    else: 
        while pvars:
            addVars(currFunc)

#Function that add variable to process table
def addVars(key):
    global currType, currFunc
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
            memo = getMemo()
            dic.addVar(key, v[0], currType, memo, lvl1, lvl2)
            dic.varPrint(key, v[0])
            pvars.pop()

    else:
        if dic.varOccupied(key,item):
            error('Variable "{}" has already been declared'.format(item))
        else:
            memo = getMemo()
            dic.addVar(key, item, currType, memo)
            dic.varPrint(key, item)
            pvars.pop()

#function that returns the memory number of the function
def getMemo():
    global currFunc, currType, progName, global_int, global_float, global_char, global_bool, local_int, local_float, local_char, local_bool
    memo = ""
    if currFunc == progName:
        if currType == 'int':
            if global_int > 1999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_int
            global_int += 1
        elif currType == 'float':
            if global_float > 2999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_float
            global_float += 1
        elif currType == 'char':
            if global_char > 3999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_char
            global_char += 1
        elif currType == 'bool':
            if global_bool > 4999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = global_bool
            global_bool += 1
        return memo
    else: 
        if currType == 'int':
            if local_int > 5999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_int
            local_int += 1
        elif currType == 'float':
            if local_float > 6999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_float
            local_float += 1
        elif currType == 'char':
            if local_char > 7999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_char
            local_char += 1
        elif currType == 'bool':
            if local_bool > 8999: 
                error('Limit of variables of type {} reached'.format(currType))
            memo = local_bool
            local_bool += 1
        return memo

#Function that return memory number of a constant
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

#Neuralgic point to add id in operand stack
def p_np_addId(p):
    'np_addId : '
    check_type_id(p[-1])
    pilaO.append(p[-1])

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
    if p[-1] not in const_table:
        aux = get_const_memo('char')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'char'
        }
    pilaO.append(const_table[p[-1]]['memo'])
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
    if p[-1] not in const_table:
        aux = get_const_memo('string')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'string'
        }
    pilaO.append(const_table[p[-1]]['memo'])
    ptypes.append('string')


#Function to check type of ids
def check_type_id(check):
    global currFunc
    if dic.varOccupied(currFunc,check):
        ptypes.append(dic.getVarType(currFunc, check))
    else: 
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

#Neuralgic point to generate assigment quadruple
def p_np_doAssign(p):
    'np_doAssign : '
    op = poper.pop()
    opdo_der = pilaO.pop()
    opdoT_der = ptypes.pop()
    temp = pilaO.pop()
    tempT = ptypes.pop()
    check = semanticCube[opdoT_der][tempT][op]
    if check != 'error':
        quadruples.append(Quadruple(op, opdo_der, None, temp))
    else:
        error('Type {} could not be assign with type {}'.format(tempT, opdoT_der))

#Neuralgic point to generate read quadruple
def p_np_addRead(p):
    'np_addRead : '
    if dic.varOccupied(currFunc, p[-1]):
        quadruples.append(Quadruple('read', None, None, p[-1]))
    else: 
        error('Variable {} not defined'.format(p[-1]))

#Neuralgic point to generate write quadruple
def p_np_addWrite(p):
    'np_addWrite : '
    if len(pilaO) > 0:
        ptypes.pop()
        opdo = pilaO.pop()
        quadruples.append(Quadruple('write', None, None, opdo))

#Neuralgic point to generate return quadruple
def p_np_addReturn(p):
    'np_addReturn : '
    if len(pilaO) > 0:
        ptypes.pop()
        opdo = pilaO.pop()
        quadruples.append(Quadruple('return', None, None, opdo))

#Neuralgic point to generate if quadruple
def p_np_checkBool(p):
    'np_checkBool : '
    check = ptypes.pop()
    if check == 'bool':
        opdo = pilaO.pop()
        quadruples.append(Quadruple('GOTOF', opdo, None, 0))
        pjumps.append(len(quadruples) - 1)
    else:
        error('Type mismatch, expected value of type bool')

def p_np_endIf(p):
    'np_endIf : '
    jump = pjumps.pop()
    quadruples[jump].temp = len(quadruples)

def p_np_else(p):
    'np_else : '
    quadruples.append(Quadruple('GOTO', None, None, 0))
    jump = pjumps.pop()
    quadruples[jump].temp = len(quadruples)
    pjumps.append(len(quadruples) - 1)

def p_np_addWhile(p):
    'np_addWhile : '
    pjumps.append(len(quadruples))

def p_np_endWhile(p):
    'np_endWhile : '
    startW = pjumps.pop()
    temp = pjumps.pop()
    quadruples.append(Quadruple('GOTO', None, None, temp))
    endW = len(quadruples)
    quadruples[startW].temp = endW

def p_np_assingFor(p):
    'np_assingFor : '
    global currVar
    op = poper.pop()
    opdo_der = pilaO.pop()
    opdoT_der = ptypes.pop()
    temp = pilaO.pop()
    tempT = ptypes.pop()
    if tempT != 'int' or opdoT_der != 'int':
        error('Expeted type int')
    else:
        currVar = temp
        quadruples.append(Quadruple(op, opdo_der, None, temp))
        ptypes.append(tempT)
        pilaO.append(temp)

def p_np_checkExp(p):
    'np_checkExp : '
    global currVar
    poper.append('>')
    generateQuad(['>'])
    pjumps.append(len(quadruples) - 1)
    check = ptypes.pop()
    if check == 'bool':
        opdo = pilaO.pop()
        quadruples.append(Quadruple('GOTOV', opdo, None, 0))
        pjumps.append(len(quadruples) - 1)
    else:
        error('Type mismatch, expected value of type bool')

def p_np_endFor(p):
    'np_endFor : '
    global currVar
    if 1 not in const_table:
        aux = get_const_memo('int')
        const_table[p[-1]] = {
            'memo': aux,
            'type': 'int'
        }
    const = (const_table[1]['memo'])
    quadruples.append(Quadruple('+=', const, None, currVar))
    startW = pjumps.pop()
    temp = pjumps.pop()
    quadruples.append(Quadruple('GOTO', None, None, temp))
    endW = len(quadruples)
    quadruples[startW].temp = endW


#Function that will generate the quadruples
def generateQuad(check):
    global currFunc
    if len(poper) > 0:
        if poper[-1] in check:
            op = poper.pop()
            opdo_der = pilaO.pop()
            opdoT_der = ptypes.pop()
            opdo_izq = pilaO.pop()
            opdoT_izq = ptypes.pop()
            tempType = semanticCube[opdoT_der][opdoT_izq][op]
            if tempType == 'error':
                error('Error trying to generate quadruple')
            else:
                temp = generate_temporal(tempType)
                pilaO.append(temp)
                ptypes.append(tempType)
                quadruples.append(Quadruple(op, opdo_izq, opdo_der, temp))
                """ BORRARFINAL
                if type(opdo_izq) == str:
                    opdo_izq = dic.get_item(currFunc).vars.get_item(opdo_izq).memo
                if type(opdo_der) == str:
                    opdo_der = dic.get_item(currFunc).vars.get_item(opdo_der).memo
                quadruples.append(Quadruple(op, opdo_izq, opdo_der, temp))"""

#function to generate temporal
def generate_temporal(tempType):
    global global_int, global_float, global_char, global_bool
    temp = ""
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
    return temp

#Function to display errors
def error(line):
    print(line)
    sys.exit()

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
print(pjumps)
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
