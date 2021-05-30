import Lex_Parser
import sys
from datastruct import DirProcess
from collections import deque
from ObjQuad import Quadruple
from re import match

d = Lex_Parser.vmHelper()

dic = d['dictionary']
const_table = d['const_table']
quadruples = d['quadruples']

fconst_table = {}
global_memory = {}
ongoing = True
current = 0
memory = {}
func = ''
write = ''
contPInt = 0
contPFloat = 0
contPChar = 0
contPBool = 0

pFuncs = deque()
pMemory = deque()
pJumps = deque()

#Fucntion to handle errors
def error(l):
    sys.exit(l)

#Flip constant table(memory -> value)
for key in const_table:
    aux = const_table[key]["memo"]
    fconst_table[aux] = key

#Get the memory adresses from the global function
global_memory = dic.getGlobalMem()

#Function that validates that a memory addres exist on a dictionary and noy have none
def exist(dic, mem, check = "no"):
    if mem not in dic:
        error("Stackoverflow")
    if dic[mem] == None and check != "no":
        error("Variable without value")

#Function that check if the function is a 
def checkMem(mem):
    if type(mem) == str:
        return getValue(int(mem[1:-1]))
    else:
        return mem

#function to get the value of a memory address
def getValue(mem):
    aux = None
    mem = checkMem(mem)
    #Global memory
    if mem >= 1000 and mem < 2000:
        exist(global_memory, mem, "yes")
        aux = int(global_memory[mem])
    elif mem >= 2000 and mem < 3000:
        exist(global_memory, mem, "yes")
        aux = float(global_memory[mem])
    elif mem >= 3000 and mem < 4000:
        exist(global_memory, mem, "yes")
        aux = global_memory[mem]
    elif mem >= 4000 and mem < 5000:
        exist(global_memory, mem, "yes")
        if global_memory[mem] == 'True' or global_memory[mem] == True:
            aux = True
        elif global_memory[mem] != None:
            aux = False

    #Local memory
    elif mem >= 5000 and mem < 6000:
        memAux = pMemory[-1]
        exist(memAux, mem, "yes")
        aux = int(memAux[mem])
    elif mem >= 6000 and mem < 7000:
        memAux = pMemory[-1]
        exist(memAux, mem, "yes")
        aux = float(memAux[mem])
    elif mem >= 7000 and mem < 8000:
        memAux = pMemory[-1]
        exist(memAux, mem, "yes")
        aux = memAux[mem]
    elif mem >= 8000 and mem < 9000:
        memAux = pMemory[-1]
        exist(memAux, mem, "yes")
        if memAux[mem] == 'True' or memAux[mem] == True:
            aux = True
        elif memAux[mem] != None:
            aux = False

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

#Function to set value to a memory address
def setValue(val, mem):
    mem = checkMem(mem)
    if mem >= 1000 and mem < 5000:
        exist(global_memory, mem)
        global_memory[mem] = val
    elif mem >= 5000 and mem < 9000:
        memAux = pMemory[-1]
        exist(memAux, mem)
        memAux[mem] = val


#Function that checks that the recive value is an int
def check_int(s):
    if s[0] in ('-'):
        return s[1:].isdigit()
    return s.isdigit()

#Function that check that the recieved value correspond with the corresponding var type
def checkValue(val, mem):
    if mem >= 1000 and mem < 2000 or  mem >= 5000 and mem < 6000:
        if not check_int(val):
            error("Expected type Int")
    if mem >= 2000 and mem < 3000 or  mem >= 6000 and mem < 7000:
        if not match(r'-?\d+\.\d+', val):
            error("Expected type Float")
    if mem >= 3000 and mem < 4000 or  mem >= 7000 and mem < 8000:
        if not match(r".", val) or len(val) > 1:
            error("Expected type Char")
    if mem >= 4000 and mem < 5000 or  mem >= 9000 and mem < 10000:
        if not match(r'(True|False)', val):
            error("Expected type Bool")

#Function that set a value for a parameter
def setParam(val, pos):
    global func, memory, contPBool, contPChar, contPFloat, contPInt
    paramT = dic.getParam(func, pos)
    if paramT == 'int':
        exist(memory, 5000+contPInt)
        memory[5000+contPInt] = val
        contPInt += 1
    if paramT == 'float':
        exist(memory, 6000+contPFloat)
        memory[6000+contPFloat] = val
        contPFloat += 1
    if paramT == 'char':
        exist(memory, 7000+contPChar)
        memory[7000+contPChar] = val
        contPChar += 1
    if paramT == 'bool':
        exist(memory, 8000+contPBool)
        memory[8000+contPBool] = val
        contPBool += 1

# ***** Execution *****
while ongoing:
    #GOTO
    if quadruples[current].getOp() == 'GOTO':
        current = quadruples[current].getTemp()
    #GOTOF
    elif quadruples[current].getOp() == 'GOTOF':
        aux = getValue(quadruples[current].getOpIzq())
        if aux:
            current += 1
        else:
            current = quadruples[current].getTemp()
    #GOTOV
    elif quadruples[current].getOp() == 'GOTOV':
        aux = getValue(quadruples[current].getOpIzq())
        if aux:
            current = quadruples[current].getTemp()
        else:
            current += 1
    #Assigment
    elif quadruples[current].getOp() == '=':
        aux = getValue(quadruples[current].getOpIzq())
        setValue(aux, quadruples[current].getTemp())
        current += 1
    #plus equal
    elif quadruples[current].getOp() == '+=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getTemp()) 
        res = aux1 + aux2
        setValue(res, quadruples[current].getTemp())
        current += 1
    #Minus equal
    elif quadruples[current].getOp() == '-=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getTemp())
        res = aux2 - aux1
        setValue(res, quadruples[current].getTemp())
        current += 1
    #Mult equal
    elif quadruples[current].getOp() == '*=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getTemp())
        res = aux1 * aux2
        setValue(res, quadruples[current].getTemp())
        current += 1
    #Division equal
    elif quadruples[current].getOp() == '/=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getTemp())
        if aux1 == 0:
            error("Division by zero")
        res = aux2 / aux1
        setValue(res, quadruples[current].getTemp())
        current += 1
    #Sum
    elif quadruples[current].getOp() == '+':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 + aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Rest
    elif quadruples[current].getOp() == '-':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 - aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Multiplication
    elif quadruples[current].getOp() == '*':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 * aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Division
    elif quadruples[current].getOp() == '/':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        if aux2 == 0:
            error("Division by zero")
        result = aux1 / aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Write
    elif quadruples[current].getOp() == 'write':
        aux = getValue(quadruples[current].getTemp())
        write = write + str(aux)
        current += 1
    #Read
    elif quadruples[current].getOp() == 'read':
        if write:
            print(write.replace(r'\n', '\n'))
            write = ''
        aux = input('Enter value: ')
        mem = checkMem(quadruples[current].getTemp())
        #print(mem)
        checkValue(aux, mem)
        setValue(aux, mem)
        current += 1
    #Greather than
    elif quadruples[current].getOp() == '>':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 > aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Less than
    elif quadruples[current].getOp() == '<':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 < aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Greather or equal than
    elif quadruples[current].getOp() == '>=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 >= aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Less or equal than
    elif quadruples[current].getOp() == '<=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 <= aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Equal
    elif quadruples[current].getOp() == '==':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 == aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Different
    elif quadruples[current].getOp() == '!=':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 != aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #And
    elif quadruples[current].getOp() == '&':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 and aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Or
    elif quadruples[current].getOp() == '|':
        aux1 = getValue(quadruples[current].getOpIzq())
        aux2 = getValue(quadruples[current].getOpDer())
        result = aux1 or aux2
        setValue(result, quadruples[current].getTemp())
        current += 1
    #Verify
    elif quadruples[current].getOp() == 'VER':
        aux = getValue(quadruples[current].getOpIzq())
        lim1 = quadruples[current].getOpDer()
        lim2 = quadruples[current].getTemp()
        if aux < lim1 or aux >= lim2:
            error("Index out of range")
        current += 1
    #ERA
    elif quadruples[current].getOp() == 'ERA':
        func = quadruples[current].getOpIzq()
        memory = dic.getLocalMem(func)
        current += 1
    #Parameter
    elif quadruples[current].getOp() == 'PARAMETER':
        aux = getValue(quadruples[current].getOpIzq())
        setParam(aux, quadruples[current].getTemp())
        current += 1
    #GOSUB
    elif quadruples[current].getOp() == 'GOSUB':
        pMemory.append(memory)
        pJumps.append(current+1)
        contPBool = 0
        contPChar = 0
        contPFloat = 0
        contPInt = 0
        current = dic.getStar(func)
        pFuncs.append(func)
    #ENDFUNC
    elif quadruples[current].getOp() == 'ENDFUNC':
        pMemory.pop()
        pFuncs.pop()
        current = pJumps.pop()
    #return
    elif quadruples[current].getOp() == 'return':
        aux = getValue(quadruples[current].getTemp())
        setValue(aux, dic.getMemAd(pFuncs[-1]))
        pMemory.pop()
        pFuncs.pop()
        current = pJumps.pop()
    #Endprogram
    elif quadruples[current].getOp() == 'END':
        ongoing = False
        if write:
            print(write.replace(r'\n', '\n'))
            write = ''
