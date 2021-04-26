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