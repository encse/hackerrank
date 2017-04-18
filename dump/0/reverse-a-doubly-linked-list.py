"""
 Reverse a doubly linked list
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None, prev_node = None):
       self.data = data
       self.next = next_node
       self.prev = prev_node

 return the head node of the updated list 
"""
def ReverseI(head):
  if head.next == None:
    head.prev = None
    return (head, head)

  (h,t) = ReverseI(head.next)
  t.next = head
  head.prev = t
  head.next = None

  return (h, head)


def Reverse(head):
  if (head == None):
    return None
  
  (h, t) = ReverseI(head)
  return h
  
  
  
  
