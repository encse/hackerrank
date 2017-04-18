s = [0,0,0]
raw_input()
s[0] = map(int,raw_input().strip().split(' '))
s[1] = map(int,raw_input().strip().split(' '))
s[2] = map(int,raw_input().strip().split(' '))
h = map(sum, s)

def ok(h):
    return h[0] == h[1] and h[1] == h[2]

z = [0,0,0]
   
while max(h) > 0 and min(h) > 0 and not ok(h):
    i = 0
    for k in xrange(1,3):
        if h[k] > h[i] or (h[k] == h[i] and s[k][z[k]] < s[i][z[i]]):
            i = k
           
    h[i] -= s[i][z[i]]
    z[i] += 1
    
print min(h)
    