"""
 Insert a node into a sorted doubly linked list
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None, prev_node = None):
       self.data = data
       self.next = next_node
       self.prev = prev_node

 return the head node of the updated list 
"""
def SortedInsert(head, data):
    n = Node(data)
    if head == None:
       return n

    h = head
    while head.next != None and data > head.next.data:
       head = head.next
    
    n.next = head.next
    if head.next != None:
        head.next.prev = n
    head.next = n
    n.prev = head
    return h
  
  
  
  
  
  
  
