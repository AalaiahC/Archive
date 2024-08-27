#lang dssl2

# HW5: Binary Heaps

interface PRIORITY_QUEUE[X]:
    # Returns the number of elements in the priority queue.
    def len(self) -> nat?
    # Returns the smallest element; error if empty.
    def find_min(self) -> X
    # Removes the smallest element; error if empty.
    def remove_min(self) -> NoneC
    # Inserts an element; error if full.
    def insert(self, element: X) -> NoneC

# Class implementing the PRIORITY_QUEUE ADT as a binary heap.
class BinHeap[X] (PRIORITY_QUEUE):
    let _data: VecC[OrC(X, NoneC)]
    let _size: nat?
    let _lt?:  FunC[X, X, bool?]
    let _capacity: nat?

    # Constructs a new binary heap with the given capacity and
    # less-than function for type X.
    def __init__(self, capacity, lt?):
        self._data = [None; capacity]
        self._size = 0
        self._lt? = lt?
        self._capacity = capacity
        
    # Returns the number of elements in the priority queue.
    def len(self) -> nat?:
        return self._size
    
    # Returns the smallest element; error if empty.
    def find_min(self) -> X:
        if self._size == 0:
            return error("Ummmmm this thing is empty...")
        else:
            return self._data[0]
            
    # Removes the smallest element; error if empty.
    def remove_min(self) -> NoneC:
        if self._size == 0: 
            error("Ummmmm this thing is empty...") 
        else: 
            let new_vec = [None; self._size - 1]
            for i in range(1, self._size):
                new_vec[i-1] = self._data[i]
            self._data = new_vec
            self._size = self._size - 1 
                       
    # Inserts an element; error if full.
    def insert(self, element: X) -> NoneC:
       if self._size == self._capacity:
           return error("Ummmmm this thing is full...")
       else:
           if self._size == 0:
               self._data = [None; 2]
               self._data[0] = element
               self._size = 1
           else:
               let b = False
               def helper(n, vec):
                if b == False: 
                    if self._data[n] == None and n == self._size: 
                        vec[n] = element
                    else:
                        if self._lt?(self._data[n], element) == False:
                            vec[n] = element 
                            b = True 
                        else: 
                            vec[n] = self._data[n] 
                else: 
                    vec[n] = self._data[n - 1] 
                    
                if n == self._data.len() - 1: 
                    return vec 
                else: 
                    helper(n + 1, vec) 
               self._data = helper(0, [None; self._data.len() + 1])
               self._size = self._size + 1

# Woefully insufficient test.
               
test 'Letters w/ None':
  
    let h = BinHeap(10, λ x, y: str(x) < str(y))
    h.insert('A')
    assert h.len() == 1
    assert h.find_min() == 'A'
    
    h.insert(None)
    assert h.len() == 2
    assert h.find_min() == 'A'
    
    h.insert('O')
    assert h.len() == 3
    assert h.find_min() == 'A'
    
    h.remove_min()
    assert h.find_min() == None
    
    h.remove_min()
    assert h.len() == 1
    assert h.find_min() == 'O'
    
    h.remove_min()
    assert h.len() == 0
    
test 'Letters2 w/ None':
  
    let h = BinHeap(10, λ x, y: str(x) < str(y))
    h.insert('A')
    h.insert(None)
    h.insert('B')
    h.insert('O')
    h.insert('P')
    assert h.find_min() == 'A'
    
    h.remove_min()
    assert h.find_min() == 'B'
    
    h.remove_min()
    assert h.find_min() == None
    
    h.remove_min()
    assert h.find_min() == 'O'
    
    h.remove_min()
    assert h.find_min() == 'P'
    
    
test 'insert, insert, remove_min':
    # The `nat?` here means our elements are restricted to `nat?`s.
    let h = BinHeap[nat?](10, λ x, y: x < y)
    h.insert(5)
    assert h.find_min() == 5
    assert h.len() == 1
    
    h.insert(3)
    assert h.find_min() == 3
    assert h.len() == 2
    
    h.insert(4)
    assert h.find_min() == 3
    assert h.len() == 3
    
    h.insert(1)
    assert h.find_min() == 1
    assert h.len() == 4
    
    h.insert(4)
    assert h.find_min() == 1
    assert h.len() == 5
    
    h.insert(4)
    assert h.find_min() == 1
    assert h.len() == 6
    
    h.remove_min()
    assert h.find_min() == 3
    assert h.len() == 5
    
    h.remove_min()
    assert h.find_min() == 4
    assert h.len() == 4
    
    h.remove_min()
    assert h.find_min() == 4
    assert h.len() == 3
    

# Sorts a vector of Xs, given a less-than function for Xs.
#
# This function performs a heap sort by inserting all of the
# elements of v into a fresh heap, then removing them in
# order and placing them back in v.
def heap_sort[X](v: VecC[X], lt?: FunC[X, X, bool?]) -> NoneC:
    let vec = BinHeap[X](v.len(), lt?)
   
    def inserting(n: nat?) -> NoneC:
        if n < v.len():
            vec.insert(v[n])
            inserting(n + 1)
    
    def removing(n: nat?) -> NoneC:
        if n < v.len():
            v[n] = vec.find_min()
            vec.remove_min()
            removing(n + 1)
            
    inserting(0)
    removing(0)

test 'heap sort descending':
    let v = [3, 6, 0, 2, 1]
    heap_sort(v, λ x, y: x > y)
    assert v == [6, 3, 2, 1, 0]
    
    let b = [1,1,1,7,8]
    heap_sort(b, λ x, y: x > y)
    assert b == [8, 7, 1, 1, 1]
    
    let n = [1,2,3,4,5]
    heap_sort(n, λ x, y: x > y)
    assert n == [5, 4, 3, 2, 1]

# Sorting by birthday.

struct person:
    let name: str?
    let birth_month: nat?
    let birth_day: nat?

def earliest_birthday() -> str?:
   let yamama1 = person("Your Mom", 3, 15) 
   let yamama2 = person("My Mom", 4, 9)
   let yamama3 = person("My Daddy's Mom", 8, 17)
   let yamama4 = person("Victor's Mom", 12, 13)
   let yamama5 = person("Jimmy's Mom", 2, 20)
   
   let yamamas = [yamama1, yamama2, yamama3, yamama4, yamama5]
   
   def whenwasyourbirthday(x, y):
       if x == None or y == None:
           return False
       else:
           return ((x.birth_month*10)+ x.birth_day) < ((y.birth_month*10)+ y.birth_day)
   heap_sort(yamamas, λ x, y: whenwasyourbirthday(x, y))
   return yamamas[0].name

test 'earliest_birthday()':
    assert earliest_birthday() == "Jimmy's Mom"
    