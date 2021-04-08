import ply.lex as lex
import ply.yacc as yacc
import sys

#Lista de todos los tokens que se utilizaran
tokens = [

    'PROGRAM',      #program
    'ID',           #id
    'SEMICOLON',     #;
    'COMMA',        #,
    'COLON',        #:
    'INT',          #cte int
    'FLOAT',        #cte float
    'STRING',       #cte string
    'L_CURPAR',     #{
    'R_CURPAR',     #}
    'EQ',           #=
    'LESS',         #<
    'GREATER',      #>
    'DIF',          #<>
    'L_PAR',        #(
    'R_PAR',        #)
    'IF',           #if
    'ELSE',         #else
    'VAR',          #var
    'PRINT',        #print
    'PLUS',         #+
    'MINUS',        #-
    'MULT',         #*
    'DIV'          #/
]
#Definicion de variables para los tokens
t_SEMICOLON = r'\;'
t_COMMA = r'\,'
t_COLON = r'\:'
t_L_CURPAR = r'\{'
t_R_CURPAR = r'\}'
t_EQ = r'\='
t_DIF = r'\<\>'
t_LESS = r'\<'
t_GREATER = r'\>'
t_L_PAR = r'\('
t_R_PAR = r'\)'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULT = r'\*'
t_DIV = r'\/'

t_ignore = ' \t'
#Definicion de tokens
def t_PROGRAM(t):
    r'program'
    t.type = 'PROGRAM'
    return t

def t_IF(t):
    r'if'
    t.type = 'IF'
    return t

def t_ELSE(t):
    r'else'
    t.type = 'ELSE'
    return t 

def t_PRINT(t):
    r'print'
    t.type = 'PRINT'
    return t 

def t_VAR(t):
    r'[a-z][a-zA-Z_0-9]*'
    t.type = 'VAR'
    return t

def t_ID(t):
    r'[A-Z][a-zA-Z0-9]*'
    t.type = 'ID'
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'"[a-zA-Z0-9!@#$%^&*()]*"'
    t.type = 'STRING'
    return t
#Funcion para contar lineas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
#Variable para manejar errores de 0 coincidencias 
def t_error(t):
    print('Line: %d, Not valid character: %r' % (t.lexer.lineno, t.value[0]))
    t.lexer.skip(1)

lexer = lex.lex()

#Funcion para probar el escaner lexico 
"""def pruebaLex():
    lexer.input("if else print + - * / gabo_125 Gabo")

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)"""
#__________PARSER____________
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
    #parser.parse(s)