import sys

class HashTable:
  #Constructor de objeto hash
  def __init__(self):
    #Maximo de valores para nuestra hash table
    self.MAX = 503
    self.dic = [None for i in range(self.MAX)]

  #Funion hash, crea un index unico dentro de nuestro arreglo a partir de una llave
  def func_hash(self, key):
    h = 0
    index = 1
    for char in key:
      h += (ord(char) * index)
      index += 1
    return h % self.MAX

  #Funcion get item, regresa el valor almacenado en la llave, en caso de haber alguno
  def get_item(self, key):
    aux = self.func_hash(key)
    return self.dic[aux]

  #Funcion delete item, elimina el valor almacenado correspondiente a una llave
  def delete_item(self, key):
    aux = self.func_hash(key)
    self.dic[aux] = None

  #Funcion is occupied, verifica si el valor ya esta almacenado
  def is_occupied(self, key):
    aux = self.func_hash(key)
    if self.dic[aux] != None:
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
    aux = self.func_hash(name)
    self.dic[aux] = Objeto(name,obj_ty, memo, lev1,lev2)


class Funcfunc:
  def __init__(self, name, func_type):
    self.name = name
    self.func_type = func_type
    self.vars = VarTab()
  
  def printFunc(self):
    print("Name:{}, Type: {}".format(self.name, self.func_type))

class DirProcess(HashTable):
# Functions  
  def addFunc(self, name, func_type):
    key = self.func_hash(name)  
    self.dic[key] = Funcfunc(name, func_type)
  
  def funcPrint(self, name):
    key = self.func_hash(name)
    self.dic[key].printFunc()

# Vars
  def getVarMemo(self, key, var):
      aux1 = self.func_hash(key)
      return self.dic[aux1].vars.get_item(var).memo

  def addVar(self, key, var, obj_ty, memo, lev1 = 1, lev2 = 1):
    aux = self.func_hash(key)
    self.dic[aux].vars.add_var(var, obj_ty, memo, lev1,lev2)

  def varPrint(self, key, var):
    aux = self.func_hash(key)
    self.dic[aux].vars.get_item(var).printObj()

  def varOccupied(self, key, var):
    aux = self.func_hash(key)
    return self.dic[aux].vars.is_occupied(var)

  def getVarType(self, key, var):
    aux = self.func_hash(key)
    return self.dic[aux].vars.get_item(var).Obj_type