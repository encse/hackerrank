# Enter your code here. Read input from STDIN. Print output to STDOUT
""" Node is defined as
class node:
  def __init__(self, data):
      self.data = data
      self.left = None
      self.right = None
"""
def traverse(root):
    res = []
    if(root != None):
        res += traverse(root.left)
        res += [root.data]
        res += traverse(root.right)
        
    return res

def check_binary_search_tree_(root):
   items = traverse(root)
   for i in xrange(0, len(items) - 1):
        if (items[i] >= items[i+1]):
            return False
   return True