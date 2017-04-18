"""
 Reverse a linked list
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the head of the linked list in the below method.
"""
def ReverseI(head):
    if (head == None):
        return None
    if (head.next == None):
        return (head, head)
    

    (h, t) = ReverseI(head.next)
    head.next = None
    t.next = head

    return (h, head)
    
    
def Reverse(head):
    (h, t) = ReverseI(head)
    return h
    
    
  
  
  
  
  
  
