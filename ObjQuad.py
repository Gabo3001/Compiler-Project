class Quadruple:
  #Constructor de objeto Quadruple
  def __init__(self, op, op_izq, op_der, temp):
    self.op = op
    self.op_izq = op_izq
    self.op_der = op_der
    self.temp = temp

  #Funion hash, crea un index unico dentro de nuestro arreglo a partir de una llave
  def get_quad(self):
      return [self.op, self.op_izq, self.op_der, self.temp]

  #Funciton that return the operator of the quadruple
  def getOp(self):
    return self.op

  #Funciton that return the left operando of the quadruple
  def getOpIzq(self):
    return self.op_izq

  #Funciton that return the right operando of the quadruple
  def getOpDer(self):
    return self.op_der

  #Funciton that return the temporal operator of the quadruple
  def getTemp(self):
    return self.temp