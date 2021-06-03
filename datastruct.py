# dataStruct.py by Gabriel Ortega and Paulina Cámara (2021)
# Program to process function and variable dictionaries

from copy import copy

class Directory:
  '''
  Class to store the main functions to use on the Function and variable table
  '''
  
  '''
  Constructor of object Dictionary
  '''
  def __init__(self):
    self.dic = {}

  '''
  Function to get an item from the dictionary

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  dictionary -> dictionary of object Funcfunc of object  VarTab
  '''
  def get_item(self, key):
    return self.dic[key]

  '''
  Verify if a key exist on the dictionary

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  bool -> states if the key is occupied or not
  '''
  def is_occupied(self, key):
    if key in self.dic:
      return True
    return False

class Objeto:
  '''
  Class used to save all the information of a variable
  '''

  '''
  Constructor of Objeto Object
  
  Parameters
  ----------
  name : str -> name of a variable
  Obj_type: str -> Type of a varibale
  memo: int -> Memory address of a variable
  level1: int -> First dimension of a variable(Default 1)
  level2: int -> Second dimension of a variable(Default 1)
  '''
  def __init__(self, name, Obj_type, memo, level1 = 1, level2 = 1):
    self.name = name
    self.Obj_type = Obj_type
    self.level1 = level1
    self.level2 = level2
    self.memo = memo

  '''
  fucntion to print all the information of a varibale
  '''
  def printObj(self):
    print("Name:{}, Type: {}, Memo:{}, Lvl1:{}, Lvl2:{}".format(self.name, self.Obj_type, self.memo, self.level1, self.level2))

class VarTab(Directory):
  '''
  Class to store all the varibales of a funciton
  Inherit Dictionary
  '''

  '''
  Function to add a variable into the dictionary

  Parameters
  ----------
  name : str -> name of a variable
  Obj_ty: str -> Type of a varibale
  memo: int -> Memory address of a variable
  lev1: int -> First dimension of a variable(Default : 1)
  lev2: int -> Second dimension of a variable(Default : 1)
  '''
  def add_var(self, name, obj_ty, memo, lev1 = 1, lev2 = 1):
    self.dic[name] = Objeto(name,obj_ty, memo, lev1,lev2)


class Funcfunc:
  '''
  Class to store the definition of a function on the dictionary
  '''

  '''
  Constructor of FuncFunc Object
  
  Parameters
  ----------
  name : str -> name of a function
  func_type: str -> Type of a function
  start: int -> Number of quadruple where the function starts (default: 0)
  '''
  def __init__(self, name, func_type, start = 0):
    self.name = name
    self.func_type = func_type
    self.start = start
    self.vars = VarTab()
    self.memAd = 0
    self.params = []
    self.memory = [0 for i in range(5)] #[Int, Float, Char, Bool, Objects]

  '''
  Fucntion to print all the information of a Fucntion
  '''
  def printFunc(self):
    print("Name:{}, Type: {}, Start: {}, Params: {}, Size: {}".format(self.name, self.func_type, self.start, self.params, self.memory))

class DirProcess(Directory):
  '''
  Class to store function used on the procces dictionary
  Inherit Dictionary
  '''

# Functions
  '''
  Add a function into the dictionary

  Parameters
  ----------
  name : str -> Key of the dictionary
  func_type: str -> Type of a function
  start: int -> Number of quadruple where the function starts (default: 0)
  '''
  def addFunc(self, name, func_type, start = 0):
    self.dic[name] = Funcfunc(name, func_type, start)

  '''
  Returns the type of a function

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  str -> type of the function
  '''
  def getFuncType(self, key):
    return self.dic[key].func_type

  '''
  Sets the reference memory addres of a function

  Parameters
  ----------
  key : str -> Key of the dictionary
  val : int -> Memory addres
  '''
  def setMemAd(self, key, val):
    self.dic[key].memAd = val

  '''
  Returns the reference memory addres of a function

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  int -> memory addres
  ''' 
  def getMemAd(self, key):
    return self.dic[key].memAd
  
  '''
  Verify if a function exist on the dictionary

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  bool -> states if the function already exist or not
  '''
  def funcOccupied(self, key):
    return self.is_occupied(key)

  '''
  Returns the parameter type of a function

  Parameters
  ----------
  key : str -> Key of the dictionary
  i: int -> index of an array

  Returns
  ----------
  str -> parameter type
  ''' 
  def funcParam(self, key, i):
    return self.dic[key].params[i-1]

  '''
  Return the numer of parameter on a function

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  int -> Number of parameter on the function
  '''
  def funcParamSize(self, key):
    return len(self.dic[key].params)

  '''
  Returns the parameter type of a function

  Parameters
  ----------
  key : str -> Key of the dictionary
  pos: int -> index of an array

  Returns
  ----------
  str -> parameter type
  ''' 
  def getParam(self, key, pos):
    return self.dic[key].params[pos-1]

  '''
  Returns the quadruple of start of a function

  Parameters
  ----------
  key : str -> Key of the dictionary

  Returns
  ----------
  int -> refernce of quadruple
  ''' 
  def getStar(self, key):
    return self.dic[key].start

  '''
  Prints all the values of a function

  Parameters
  ----------
  name : str -> Key of the dictionary
  '''
  def funcPrint(self, name):
    self.dic[name].printFunc()

# Vars
  '''
  Returns the memory addres of a variable

  Parameters
  ----------
  key : str -> name of function
  var : str -> name of the variable 

  Returns
  ----------
  int -> memory addres of a variable
  ''' 
  def getVarMemo(self, key, var):
    if type(var) == str and var[0] == '(' and var[-1] == ')':
      return var
    return self.dic[key].vars.get_item(var).memo

  '''
  Add a variable into the dictionary

  Parameters
  ----------
  key : str -> Name of the fucntion
  var : str -> Name of the variable
  obj_type : str -> Type of a variable
  memo : int -> Memory address of a function
  lev1: int -> First dimension of a variable(Default : 1)
  lev2: int -> Second dimension of a variable(Default : 1)
  '''
  def addVar(self, key, var, obj_ty, memo, lev1 = 1, lev2 = 1):
    self.dic[key].vars.add_var(var, obj_ty, memo, lev1,lev2)

  '''
  Prints all the values of a variable

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable
  '''
  def varPrint(self, key, var):
    self.dic[key].vars.get_item(var).printObj()

  '''
  Checks  if a variable exist on the dictionary

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  bool : states if the variable exists or not
  '''
  def varOccupied(self, key, var):
    return self.dic[key].vars.is_occupied(var)

  '''
  Returns the type of a variable

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  str : Type of the variable
  '''
  def getVarType(self, key, var):
    return self.dic[key].vars.get_item(var).Obj_type

  '''
  Returns the first dimension of a variable

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  int : first dimension of the variable
  '''
  def getLvl1(self, key, var):
    return self.dic[key].vars.get_item(var).level1

  '''
  Returns the second dimension of a variable

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  int : second dimension of the variable
  '''
  def getLvl2(self, key, var):
    return self.dic[key].vars.get_item(var).level2

  '''
  Checks  if an array exist on the dictionary

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  bool : states if the array exists or not
  '''
  def chechArr(self, key, mem):
    if mem in self.dic[key].vars.dic.keys():
      return self.isArr(key, mem)
    else:
      return False

  '''
  Checks  if a variable is an array

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  bool : states if the variable is an array or not
  '''
  def isArr(self, key, var):
    if self.dic[key].vars.get_item(var).level1 > 1 or self.dic[key].vars.get_item(var).level2 > 1:
      return True
    else:
      return False

  '''
  checks if a variable is a one dimensional array

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  bool : states if the variable is a one dimensional array or not
  '''
  def checkOneDim(self, key, var):
    if self.dic[key].vars.get_item(var).level2 > 1:
      return False
    else:
      return True

  '''
  checks if a variable is a two dimensional array

  Parameters
  ----------
  key : str -> Name of the function
  var : str -> Name of the variable

  Returns
  ----------
  bool : states if the variable is a two dimensional array or not
  '''
  def checkTwoDim(self, key, var):
    if self.dic[key].vars.get_item(var).level2 > 1:
      return True
    else:
      return False

  '''
  Deletes all the information of the variables in a function

  Parameters
  ----------
  key : str -> Name of the function
  '''
  def delVar(self, key):
    del self.dic[key].vars

  '''
  Returns the type of a variable from its memory addres

  Parameters
  ----------
  key : str -> Name of the function
  mem : str -> Memory addres

  Returns
  ----------
  int : Memory addres
  '''
  def getTypeFromMemo(self, key, mem):
    for var in self.dic[key].vars.dic:
      if self.getVarMemo(key, var) == mem:
        return self.getVarType(key, var)

#Parameters
  '''
  Add a parameter to the parameter list of a function

  Parameters
  ----------
  name : str -> Name of the function
  type : str -> Type of the parameter
  '''
  def addParam(self, name, type):
    self.dic[name].params.append(type)

#Others
  '''
  Prints all the infomation of all the functions on the dictionaory
  '''
  def printAll(self):
    for key in self.dic:
      self.funcPrint(key)

  '''
  Prints all the infomation of all the variables on a fucntion

  Parameters
  ----------
  key : str -> Name of the function
  '''
  def printFunc(self, key):
    self.funcPrint(key)
    for var in self.dic[key].vars.dic:
      self.varPrint(key, var)
  
  '''
  Returns a dictionary that will serve as the global memory for the VM

  Returns
  ----------
  dictionary : dictionary that will serve as the global memory for the vm
  '''
  def getGlobalMem(self):
    aux = {}
    for key in self.dic:
      if self.dic[key].func_type == "program" or self.dic[key].func_type == "class":
        #int
        for i in range(self.dic[key].memory[0]):
          aux[1000+i] = None
        #float
        for i in range(self.dic[key].memory[1]):
          aux[2000+i] = None
        #char
        for i in range(self.dic[key].memory[2]):
          aux[3000+i] = None
        #bool
        for i in range(self.dic[key].memory[3]):
          aux[4000+i] = None
        if type(self.dic[key].memory[4]) != int:
          for i in self.dic[key].memory[4]:
            aux[i[0]] = None
    return aux

  '''
  Returns a dictionary that will serve as a local memory for the VM

  Parameters
  ----------
  func : str -> Name of the function

  Returns
  ----------
  dictionary : dictionary that will serve as a local memory for the vm
  '''
  def getLocalMem(self, func):
    aux = {}
    for key in self.dic:
      if key == func:
        #int
        for i in range(self.dic[key].memory[0]):
          aux[5000+i] = None
        #float
        for i in range(self.dic[key].memory[1]):
          aux[6000+i] = None
        #char
        for i in range(self.dic[key].memory[2]):
          aux[7000+i] = None
        #bool
        for i in range(self.dic[key].memory[3]):
          aux[8000+i] = None
    return aux

  '''
  Returns a dictionary for a class

  Parameters
  ----------
  dictionary : dictionay -> Dictionary were the class dictionary will be saved
  arr : list -> list of clases

  Returns
  ----------
  dictionary : dictionary for a class
  '''
  def createClassMemo(self, dictionary, arr):
    for key in self.dic:
      if self.dic[key].func_type == "program" or self.dic[key].func_type == "class":
        for i in self.dic[key].memory[4]:
          for j in arr:
            if i[1] == j[0]:
              dictionary[i[0]] = j[1].getGlobalMem()
    return dictionary

  '''
  Copies the information of another dictionary into the actual dictionary

  Parameters
  ----------
  auxDic : dictionay -> Dictionary of the Father Class
  son : str -> Name of son Class
  father : str -> Name of father Class
  '''
  def getCopy(self, auxDic, son, father):
    for v in auxDic.dic[father].vars.dic:
      self.dic[son].vars.dic[v] = auxDic.dic[father].vars.dic[v]
    for i in range(4):
      self.dic[son].memory[i] = copy(auxDic.dic[father].memory[i])
    for key in auxDic.dic:
      if key != father:
        self.dic[key] = auxDic.dic[key]
  
  '''
  Fucntion to clear all the information of the dictionary
  '''
  def clearDic(self):
    self.dic = {}