#Body
"""
 Get Node data of the Nth Node from the end.
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the node data of the linked list in the below method.
"""
def getNodeI(head, position):
    if (head == None):
      return (position, None)
    (p, n) = getNodeI(head.next, position)
    if (p == 0):
        return (p-1, head)
    else:
        return (p-1, n)
            
def GetNode(head, position):
  (x, h) = getNodeI(head, position)
  return h.data
  
  
  
  
  
  
  
  
  
