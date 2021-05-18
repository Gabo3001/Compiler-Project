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

#Flip constant table(memory -> value)
for key in const_table:
    aux = const_table[key]["memo"]
    fconst_table[aux] = key

#Get the memory adresses from the global function
global_memory = dic.getGlobalMem()



