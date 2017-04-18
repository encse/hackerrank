"""
 Find the node at which both lists merge and return the data of that node.
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 
"""
def Contains(headA, headB):
    if(headB == None):
        return False
    if(headA == headB):
        return True
    return Contains(headA, headB.next)

def FindMergeNode(headA, headB):
    if(Contains(headA, headB)):
        return headA.data
    return FindMergeNode(headA.next, headB)
    
  
  
  
  
  
  
  
  
  
