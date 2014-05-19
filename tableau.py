import math

def num_syt(shape):
  """
  Compute the number of standard Young tableau on `shape` via the
  Frame-Robinson-Thrall hooklength formula.
  """
  diagram = Young(shape)
  res = math.factorial(sum(shape))
  for i in range(len(diagram.shape)):
    for j in range(diagram.shape[i]):
      res = res // diagram.hooklength((i,j))
  return res

def promotion_order(tableau):
  """
  Compute the order of the promotion operator on a given tableau.
  """
  original_tableau = Young(tableau.shape, tableau.filling)
  tableau.promotion()
  count = 1
  print("\nPromotion {0}:".format(count))
  tableau.show()
  while not tableau == original_tableau:
    tableau.promotion()
    count += 1
    print("\nPromotion {0}:".format(count))
    tableau.show()
  return count

class Young:
  def __init__(self, shape, filling = None):
    for i in range(1,len(shape)):
      if shape[i-1] < shape[i]:
        raise Exception()
    self.shape = shape
    self.filling = [[0 for i in range(rowlen)] for rowlen in shape]
    if filling != None:
      for i in range(len(filling)):
        for j in range(len(filling[i])):
          self.filling[i][j] = filling[i][j]

  def __eq__(self,other):
    if self.shape != other.shape:
      return False
    return self.filling == other.filling

  def __cmp__(self,other):
    """
    Compare two Young Diagrams via their shape.
    """
    if self.shape == other.shape:
      return 0
    for i in range(len(self.shape)):
      if len(other.shape) < i or self.shape[i] > other.shape[i]:
        return 1
      elif self.shape[i] < other.shape[i]:
        return -1

  def in_diagram(self, posn):
    i,j = posn
    return i < len(self.shape) and j < self.shape[i]

  def is_corner(self, posn):
    """
    Check whether or not a position posn = (i,j) is an inner corner, i.e.
    there is no box in the diagram to the right or below of posn.
    """
    i,j = posn
    return not self.in_diagram((i+1,j)) and not self.in_diagram((i,j+1))

  def to_string(self):
    res = ""
    row_string = ""
    for row in self.filling:
      for entry in row:
        row_string += str(entry) + " "
      res += row_string
      row_string = "\n"
    return res

  def show(self):
    print(self.to_string())

  def ballot_word(self):
    """
    Compute the associated ballot word as a list of integers.
    """
    if self.filling == None or not self.is_standard():
      return None
    else:
      ballot_word = []
      for n in range(1,sum(self.shape)+1):
        for (i,row) in enumerate(self.filling):
          if n in row:
            ballot_word.append(i+1)
            break
    return ballot_word

  def hooklength(self,posn):
    """
    Computes the hooklength of the box at posn = (i,j).
    """
    if self.in_diagram(posn):
      i,j = posn
      return len(filter(lambda k: k > j, self.shape[i:])) + (self.shape[i] - (j+1))
    else:
      return 0


  def is_standard(self):
    for row in self.filling:
      for i in range(1,len(row)):
        if row[i-1] >= row[i]:
          return False
    for i in range(1,len(self.shape)):
      for j in range(self.shape[i]):
        if self.filling[i-1][j] >= self.filling[i][j]:
          return False
    return True

  def is_semistandard(self):
    for row in self.filling:
      for i in range(1,len(row)):
        if row[i-1] > row[i]:
          return False
    for i in range(1,len(self.shape)):
      for j in range(self.shape[i]):
        if self.filling[i-1][j] > self.filling[i][j]:
          return False
    return True

  def promotion(self):
    """
    Apply the Schensted promotion operation.
    """
    # First decrease all entries
    for i in range(len(self.filling)):
      for j in range(len(self.filling[i])):
        self.filling[i][j] -= 1

    # Slide the zero tile through
    empty_tile = (0,0)
    promotion_path = [empty_tile]
    while not self.is_corner(empty_tile):
      i,j = empty_tile
      if self.in_diagram((i+1,j)) and self.in_diagram((i,j+1)):
        if self.filling[i+1][j] <= self.filling[i][j+1]:
          self.filling[i][j] = self.filling[i+1][j]
          empty_tile = (i+1,j)
        else:
          self.filling[i][j] = self.filling[i][j+1]
          empty_tile = (i,j+1)
      elif self.in_diagram((i+1,j)):
        self.filling[i][j] = self.filling[i+1][j]
        empty_tile = (i+1,j)
      else:
        self.filling[i][j] = self.filling[i][j+1]
        empty_tile = (i,j+1)
      promotion_path.append(empty_tile)
    self.filling[empty_tile[0]][empty_tile[1]] = sum(self.shape)
    return promotion_path

  def evacuation(self):
    """
    Apply the evacuation, or Schensted involution, to the given tableau.
    """
    current_shape = tuple((self.shape[i] for i in range(len(self.shape))))
    while sum(current_shape) > 0:
      subfilling = [[self.filling[i][j] for j in range(current_shape[i])]
                    for i in range(len(current_shape))]
      subtableau = Young(current_shape, subfilling)
      evac_path = subtableau.promotion()
      i,j = evac_path.pop(0)
      while evac_path != []:
        k,l = evac_path.pop(0)
        self.filling[i][j] = self.filling[k][l]
        i,j = k,l

      self.filling[i][j] = sum(current_shape)
      if j == 0:
        current_shape = current_shape[:-1]
      else:
        tmp = list(current_shape)
        tmp[i] -= 1
        current_shape = tuple(tmp)
      for i in range(len(current_shape)):
        for j in range(current_shape[i]):
          self.filling[i][j] -= 1


def tests():
  young = Young((3,2), [[1,2,3],[4,5]])
  young2 = Young((4,4,2,1), [[1,2,4,10],[3,5,8,11],[6,7],[9]])
  young3 = Young((3,2), [[1,2,4],[3,5]])

  assert young.to_string() == "1 2 3 \n4 5 "
  assert young.is_standard()
  assert young.is_semistandard()
  assert (young < young2)
  assert not (young2 < young)
  assert not (young < young)
  assert num_syt((4,3,3,2)) == 2970
  assert num_syt((1,)) == 1

tests()

print(num_syt((5,5,5,5)))

young = Young((4,2,1,1),[[1,2,3,4],[5,6],[7],[8]])
print(promotion_order(young))
