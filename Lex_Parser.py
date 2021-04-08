import ply.lex as lex
import ply.yacc as yacc
import sys

#List of Tokens
tokens = [

    'PROGRAM',      #program
    'ID',           #id
    'CLASS',        #Class
    'INHERIT',      #Inherit
    'ATTRIBUTES',   #Attributes
    'METHODS',      #Methods
    'VARS',         #Global variables(vars)
    'VAR',          #variables (var)
    'FUCTION',      #Function
    'MAIN',         #Main
    'READ',         #Read
    'WRITE',        #Write
    'RETURN',       #Return
    'IF',           #if
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
    'STRING',       #string
    'CTE_INT',      #Cte.int
    'CTE_FLOAT',    #Cte.float
    'CTE_STRING',   #Cte.string
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
#Definition of tokens
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

def t_CTE_STRING(t):
    r'"[a-zA-Z0-9!@#$%^&*()]*"'
    t.type = 'CTE_STRING'
    return t

#Function to count lines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
#Function to show lexical error 
def t_error(t):
    print('Line: %d, Not valid character: %r' % (t.lexer.lineno, t.value[0]))
    t.lexer.skip(1)

lexer = lex.lex()

#Funcion para probar el escaner lexico 
"""def pruebaLex():
    lexer.input('if else print + - "HOLA" program vars 123 123.1 -123.5 class * / gabo_125 Gabo')

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

pruebaLex()"""

"""#__________PARSER____________
#Definicion de gramatica
def p_program(p):
    '''
    program  : PROGRAM ID SEMICOLON programT
    
    programT : vars programF
             | programF
    
    programF : bloque empty
    '''
    p[0] = None

def p_vars(p):
    '''
    vars  : VAR varsT
    
    varsT : ID COMMA varsT
          | ID COLON tipo SEMICOLON varsF
    
    varsF : varsT
          | empty
    '''
    p[0] = None

def p_tipo(p):
    '''
    tipo  : INT empty
          | FLOAT empty
    '''
    p[0] = None

def p_bloque(p):
    '''
    bloque  : L_CURPAR bloqueT
    bloqueT : estatuto bloqueT
            | R_CURPAR  empty
    '''
    p[0] = None

def p_estatuto(p):
    '''
    estatuto  : asignacion empty
              | condicion empty
              | escritura empty
    '''
    p[0] = None

def p_asignacion(p):
    '''
    asignacion  : ID EQ expresion SEMICOLON empty
    '''
    p[0] = None
    
def p_escritura(p):
    '''
    escritura  : PRINT L_PAR escrituraT
    escrituraT : expresion escrituraF
               | STRING escrituraF
    escrituraF : COMMA  escrituraT
               | R_PAR SEMICOLON empty
    '''
    p[0] = None

def p_expresion(p):
    '''
    expresion  : exp expresionT
    expresionT : LESS exp empty
               | GREATER exp empty
               | DIF exp empty
               | empty
    '''
    p[0] = None

def p_condicion(p):
    '''
    condicion  : IF L_PAR expresion R_PAR bloque condicionT
    condicionT : ELSE bloque empty
               | empty
    '''
    p[0] = None

def p_exp(p):
    '''
    exp  : termino expT
    expT : PLUS exp
         | MINUS exp
         | empty
    '''
    p[0] = None

def p_termino(p):
    '''
    termino  : factor terminoT
    terminoT : MULT termino
         | DIV termino
         | empty
    '''
    p[0] = None

def p_factor(p):
    '''
    factor  : L_PAR expresion R_PAR empty
            | factorT
    factorT : PLUS factorF
            | MINUS factorF
            | factorF
    factorF : varcte empty
    '''
    p[0] = None

def p_varcte(p):
    '''
    varcte  : ID empty
            | INT empty
            | FLOAT empty
    '''
    p[0] = None
#Funcion de manejo de errores
def p_error(p):
    print("Syntax error found at line %d." % (lexer.lineno))

def p_empty(p):
    '''
    empty : 
    '''
    p[0] = None

parser = yacc.yacc()

try:
    f = open("pruebas.txt", "r")
    for s in f:
        parser.parse(s)
    #parser.parse(f.read())
except EOFError:
    print('Error')
    #parser.parse(s)"""