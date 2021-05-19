import Lex_Parser
import sys
from datastruct import DirProcess
from collections import deque
from ObjQuad import Quadruple

d = Lex_Parser.vmHelper()

dic = d['dictionary']
const_table = d['const_table']
quadruples = d['quadruples']

fconst_table = {}
global_memory = {}
ongoing = True
current = 0

#BORRAR_____________________________________________________________
# for item in quadruples:     
#     print(item.get_quad())

#Fucntion to handle errors
def error(l):
    sys.exit(l)

#Flip constant table(memory -> value)
for key in const_table:
    aux = const_table[key]["memo"]
    fconst_table[aux] = key

#Get the memory adresses from the global function
global_memory = dic.getGlobalMem()

def getValue(mem):
    aux = None
    #Global memory
    if mem >= 1000 and mem < 2000:
        aux = int(global_memory[mem])
    elif mem >= 2000 and mem < 3000:
        aux = float(global_memory[mem])
    elif mem >= 3000 and mem < 4000:
        aux = global_memory[mem]
    elif mem >= 4000 and mem < 5000:
        if global_memory[mem] == 'True' or global_memory[mem] == True:
            aux = True
        elif global_memory[mem] != None:
            aux = False

    #Local memory
    # elif mem >= 5000 and mem < 6000:
    #     aux = 
    # elif mem >= 6000 and mem < 7000:
    #     aux = 
    # elif mem >= 7000 and mem < 8000:
    #     aux = 
    # elif mem >= 8000 and mem < 9000:
    #     aux = 

    #constant variables
    elif mem >= 9000 and mem < 10000:
        aux = fconst_table[mem]
    elif mem >= 10000 and mem < 11000:
        aux = fconst_table[mem]
    elif mem >= 11000 and mem < 12000:
        aux = fconst_table[mem]
    elif mem >= 12000 and mem < 13000:
        if fconst_table[mem] == 'True':
            aux = True
        elif fconst_table[mem] != None:
            aux = False
    elif mem >= 13000 and mem < 14000:
        aux = fconst_table[mem]
    
    if aux is None:
        error("Not assign variable")

    return aux

def setValue(val, mem):
    if mem >= 1000 and mem < 5000:
        global_memory[mem] = val
    # else:
    #     MEMORIA LOCAL

while ongoing:
    if quadruples[current].getOp() == 'GOTO':
        current = quadruples[current].getTemp()
    elif quadruples[current].getOp() == '=':
        aux = getValue(quadruples[current].getOpIzq())
        setValue(aux, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == '+':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 + aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == '-':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 - aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == '*':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 * aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == '/':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        if aux2 == 0:
            error("Division by zero")
        result = aux1 / aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == 'write':
        aux = getValue(quadruples[current].getTemp())
        print(aux)
        current += 1
    elif quadruples[current].getOp() == 'read':
        aux = input('Enter value: ')
        setValue(aux, quadruples[current].getTemp())
        current += 1
    elif quadruples[current].getOp() == 'END':
        ongoing = False
