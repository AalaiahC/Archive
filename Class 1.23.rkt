#lang dssl2

# A Link is one of:
# - cons { data: Any, next: Link }
# - None

struct cons:
    let data
    let next

#
class SLL: # SLL = Slightly-Linked List
    let head
    
    def __init__(self):
        self.head = None
        
    def get_first(self):
        if cons?(self.head): return self.head.data
        else: error('empty list')
        
    def get_nth(self, n):
        let curr = self.head
        while not curr == None:
            if n == 0:
                return curr.data
            n    = n - 1
            curr = curr.next
        error('list too short')
        
    def _find_nth_node(self, n):
        let curr = self.head
        while not curr == None:
            if n == 0:
                return curr
            n = n - 1
            curr = curr.next
        error('list too short')
        
    def get_nthh(self, n):
        return self._find_nth_node(n).data
        
    def set_nth(self, n, val):
        self._find_nth_node(n).data = val
        
let foo = SLL()
foo.get_first()