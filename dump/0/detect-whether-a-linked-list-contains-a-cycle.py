def has_cycle(head):
    if  head == None:  
        return False
    p = head
    q = p.next
    while q != None:
        if (p == q):
            return True
        q = q.next
        #if q == None:
        #    return False
        q = q.next
        p = p.next
    return False