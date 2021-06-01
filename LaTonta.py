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
arrClases = d["arrClases"]

fconst_table = {}
global_memory = {}
aux_global_memory = {}
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
if 0 in global_memory.keys():
    global_memory = dic.createClassMemo(global_memory, arrClases)


#Function that validates that a memory addres exist on a dictionary and noy have none
def exist(dic, mem, check = "no"):
    if mem not in dic:
        error("Stackoverflow")
    if dic[mem] == None and check != "no":
        error("Variable without value")

#Function that check if the function is a 
def checkMem(mem):
    if type(mem) == str and mem[0] == '(':
        return getValue(int(mem[1:-1]))
    elif type(mem) == str:
        aux = mem.split('.')
        return int(aux[0])
    else:
        return mem

#function to get the value of a memory address
def getValue(mem):
    aux = None
    fmem = checkMem(mem)
    #Global memory
    if fmem >= 1000 and fmem < 2000:
        exist(global_memory, fmem, "yes")
        aux = int(global_memory[fmem])
    elif fmem >= 2000 and fmem < 3000:
        exist(global_memory, fmem, "yes")
        aux = float(global_memory[fmem])
    elif fmem >= 3000 and fmem < 4000:
        exist(global_memory, fmem, "yes")
        aux = global_memory[fmem]
    elif fmem >= 4000 and fmem < 5000:
        exist(global_memory, fmem, "yes")
        if global_memory[fmem] == 'True' or global_memory[fmem] == True:
            aux = True
        elif global_memory[fmem] != None:
            aux = False
    elif fmem >= 0 and fmem < 500:
        exist(global_memory, fmem, "yes")
        secondmem = mem.split('.')
        exist(global_memory[fmem], int(secondmem[1]), "yes")
        aux = global_memory[fmem][int(secondmem[1])]

    #Local memory
    elif fmem >= 5000 and fmem < 6000:
        memAux = pMemory[-1]
        exist(memAux, fmem, "yes")
        aux = int(memAux[fmem])
    elif fmem >= 6000 and fmem < 7000:
        memAux = pMemory[-1]
        exist(memAux, fmem, "yes")
        aux = float(memAux[fmem])
    elif fmem >= 7000 and fmem < 8000:
        memAux = pMemory[-1]
        exist(memAux, fmem, "yes")
        aux = memAux[fmem]
    elif fmem >= 8000 and fmem < 9000:
        memAux = pMemory[-1]
        exist(memAux, fmem, "yes")
        if memAux[fmem] == 'True' or memAux[fmem] == True:
            aux = True
        elif memAux[fmem] != None:
            aux = False

    #constant variables
    elif fmem >= 9000 and fmem < 10000:
        aux = fconst_table[fmem]
    elif fmem >= 10000 and fmem < 11000:
        aux = fconst_table[fmem]
    elif fmem >= 11000 and fmem < 12000:
        aux = fconst_table[fmem]
    elif fmem >= 12000 and fmem < 13000:
        if fconst_table[fmem] == 'True':
            aux = True
        elif fconst_table[fmem] != None:
            aux = False
    elif fmem >= 13000 and fmem < 14000:
        aux = fconst_table[fmem]
    
    if aux is None:
        error("Not assign variable")

    return aux

#Function to set value to a memory address
def setValue(val, mem):
    fmem = checkMem(mem)
    if fmem >= 1000 and fmem < 5000:
        exist(global_memory, fmem)
        global_memory[fmem] = val
    elif fmem >= 5000 and fmem < 9000:
        memAux = pMemory[-1]
        exist(memAux, fmem)
        memAux[fmem] = val
    elif fmem >= 0 and fmem < 500:
        exist(global_memory, fmem)
        secondmem = mem.split('.')
        exist(global_memory[fmem], int(secondmem[1]))
        global_memory[fmem][int(secondmem[1])] = val


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
    if "." in func:
        splitFunc = func.split('.')
        for i in arrClases:
            if i[0] == splitFunc[1]:
                paramT = i[1].getParam(splitFunc[2], pos)
    else:
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
        if "." in func:
            splitFunc = func.split('.')
            for i in arrClases:
                if i[0] == splitFunc[1]:
                    memory = i[1].getLocalMem(splitFunc[2])
        else:
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
        if "." in func:
            splitFunc = func.split('.')
            aux_global_memory = global_memory
            global_memory = aux_global_memory[int(splitFunc[0])]
            for i in arrClases:
                if i[0] == splitFunc[1]:
                    current = i[1].getStar(splitFunc[2])
        else:
            current = dic.getStar(func)

        contPBool = 0
        contPChar = 0
        contPFloat = 0
        contPInt = 0
        pFuncs.append(func)
    #ENDFUNC
    elif quadruples[current].getOp() == 'ENDFUNC':
        pMemory.pop()
        tempfunc = pFuncs.pop()
        if "." in tempfunc:
            global_memory = aux_global_memory
        current = pJumps.pop()
    #return
    elif quadruples[current].getOp() == 'return':
        aux = getValue(quadruples[current].getTemp())
        if "." in pFuncs[-1]:
            splitFunc = pFuncs[-1].split('.')
            for i in arrClases:
                if i[0] == splitFunc[1]:
                    setValue(aux, i[1].getMemAd(splitFunc[2]))
            global_memory = aux_global_memory
        else:
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
