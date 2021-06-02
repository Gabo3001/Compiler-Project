class Quadruple:
  '''
  Class used to save all the sections of a quadruple
  '''

  '''
  Constructor of Quadruple Object
  
  Parameters
  ----------
  op : int -> Operator of the quadruple
  op_izq: int -> Left operand of the quadruple
  op_der: int -> Right operand of the quadruple
  temp_: int -> Temporal opderand of the quadruple 
  '''
  def __init__(self, op, op_izq, op_der, temp):
    self.op = op
    self.op_izq = op_izq
    self.op_der = op_der
    self.temp = temp

  '''
  Fucntion that return the generated quadruple
  
  Returns
  ----------
  list -> A list with all four elements of a quadruple
  '''
  def get_quad(self):
      return [self.op, self.op_izq, self.op_der, self.temp]

  '''
  Funciton that return the operator of the quadruple
  
  Returns
  ----------
  int -> The operator of the quadruple
  '''
  def getOp(self):
    return self.op

  '''
  Funciton that return the left operand of the quadruple
  
  Returns
  ----------
  int -> The left operand of the quadruple
  '''
  def getOpIzq(self):
    return self.op_izq

  '''
  Funciton that return the right operando of the quadruple
  
  Returns
  ----------
  int -> The rigth operand of the quadruple
  '''
  def getOpDer(self):
    return self.op_der

  '''
  Funciton that return the temporal operator of the quadruple
  
  Returns
  ----------
  int -> The temporal operand of the quadruple
  '''
  def getTemp(self):
    return self.temp