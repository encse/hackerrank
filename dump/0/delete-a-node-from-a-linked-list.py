"""
 Delete Node at a given position in a linked list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the head of the linked list in the below method. 
"""

def Delete(head, position):
    prev = None
    curr = head
    while(position > 0):
        prev = curr
        curr = curr.next
        position -= 1
        
    if(prev == None):
        head = curr.next
    else:
        prev.next = curr.next
    return head
        
  
  
  
  
  
