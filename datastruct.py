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

  #Funcion add item, almacena un valor a partir del resultado de nuestra función hash
  def add_item(self, key, val):
    aux = self.func_hash(key)
    self.dic[aux] =  val

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
  def __init__(self, name, Obj_type, level1 = 1, level2 = 1):
    self.name = name
    self.Obj_type = Obj_type
    self.level1 = level1
    self.level2 = level2

  def printObj(self):
    print("Name:{}, Type: {}, Lvl1:{}, Lvl2:{}".format(self.name, self.Obj_type, self.level1, self.level2))

class VarTab(HashTable):
  def add_var(self, name, obj_ty, lev1 = 1, lev2 = 1):
    aux = self.func_hash(name)
    self.dic[aux] = Objeto(name,obj_ty,lev1,lev2)


class Funcfunc:
  def __init__(self, name, func_type):
    self.name = name
    self.func_type = func_type
    self.vars = VarTab()
  
  def printFunc(self):
    print("Name:{}, Type: {}".format(self.name, self.func_type))

class DirProcess(HashTable):
  def add_process(self, line):
    aux = line.split(":")
    name = ""
    prosT = ""

    #agregr pross
    if aux[0] == "vars":
      name = "Program"
      prosT = "program"
    else:
      s = aux[0].split("(")
      name = s[0]
      prosT = s[1].replace(')', '') 

    key = self.func_hash(name)
    self.dic[key] = Funcfunc(name, prosT)
    #self.dic[key].printFunc()

    # agregar variables
    aux2 = list(reversed(aux[1].split(".")))
    vtype = ""

    for item in aux2:
      if item[0] == "(":
        item = item.replace('(', '') 
        vtype = item.replace(')', '') 

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
        self.dic[key].vars.add_var(v[0], vtype, lvl1, lvl2)
       # self.dic[key].vars.get_item(v[0]).printObj()

      else:
        self.dic[key].vars.add_var(item, vtype)
        #self.dic[key].vars.get_item(item).printObj()

  def add_prog(self, line):
    key = self.func_hash("Program")
    if self.is_occupied("Program"):
      self.dic[key].name = line
    else:
      self.dic[key] = Funcfunc(line, "program")
    #self.dic[key].printFunc()