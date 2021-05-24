class HashTable:
  #Constructor de objeto hash
  def __init__(self):
    self.dic = {}

  #Funcion get item, regresa el valor almacenado en la llave, en caso de haber alguno
  def get_item(self, key):
    return self.dic[key]

  #Funcion is occupied, verifica si el valor ya esta almacenado
  def is_occupied(self, key):
    if key in self.dic:
      return True
    return False

class Objeto:
  def __init__(self, name, Obj_type, memo, level1 = 1, level2 = 1):
    self.name = name
    self.Obj_type = Obj_type
    self.level1 = level1
    self.level2 = level2
    self.memo = memo

  def printObj(self):
    print("Name:{}, Type: {}, Memo:{}, Lvl1:{}, Lvl2:{}".format(self.name, self.Obj_type, self.memo, self.level1, self.level2))

class VarTab(HashTable):
  def add_var(self, name, obj_ty, memo, lev1 = 1, lev2 = 1):
    self.dic[name] = Objeto(name,obj_ty, memo, lev1,lev2)


class Funcfunc:
  def __init__(self, name, func_type, start = 0):
    self.name = name
    self.func_type = func_type
    self.start = start
    self.vars = VarTab()
    self.params = []
    self.memory = [0 for i in range(4)] #[Int, Float, Char, Bool]
  
  def printFunc(self):
    print("Name:{}, Type: {}, Start: {}, Params: {}, Size: {}".format(self.name, self.func_type, self.start, self.params, self.memory))

class DirProcess(HashTable):
# Functions  
  def addFunc(self, name, func_type, start = 0):
    self.dic[name] = Funcfunc(name, func_type, start)

  def funcOccupied(self, key):
    return self.is_occupied(key)

  def funcParam(self, key, i):
    return self.dic[key].params[i-1]

  def funcParamSize(self, key):
    return len(self.dic[key].params)

  def funcPrint(self, name):
    self.dic[name].printFunc()

# Vars
  def getVarMemo(self, key, var):
      return self.dic[key].vars.get_item(var).memo

  def addVar(self, key, var, obj_ty, memo, lev1 = 1, lev2 = 1):
    self.dic[key].vars.add_var(var, obj_ty, memo, lev1,lev2)

  def varPrint(self, key, var):
    self.dic[key].vars.get_item(var).printObj()

  def varOccupied(self, key, var):
    return self.dic[key].vars.is_occupied(var)

  def getVarType(self, key, var):
    return self.dic[key].vars.get_item(var).Obj_type
    self.dic[key].vars[var].printObj()

  def delVar(self, key):
    del self.dic[key].vars

#Parameters
  def addParam(self, name, type):
    self.dic[name].params.append(type)

#Others
  def printAll(self):
    for key in self.dic:
      self.funcPrint(key)
      if 'self.dic[key].vars' in globals():
        for var in self.dic[key].vars.dic:
          self.varPrint(key, var)

  def printFunc(self, key):
    self.funcPrint(key)
    for var in self.dic[key].vars.dic:
      self.varPrint(key, var)
  
  def getGlobalMem(self):
    aux = {}
    for key in self.dic:
      if self.dic[key].func_type == "program":
        for var in self.dic[key].vars.dic:
          aux[self.getVarMemo(key, var)] = None
    return aux
    