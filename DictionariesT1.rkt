#lang dssl2

# HW3: Dictionaries

import sbox_hash

# A signature for the dictionary ADT. The contract parameters `K` and
# `V` are the key and value types of the dictionary, respectively.
interface DICT[K, V]:
    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?
    # Is the given key mapped by the dictionary?
    def mem?(self, key: K) -> bool?
    # Gets the value associated with the given key; calls `error` if the
    # key is not present.
    def get(self, key: K) -> V
    # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC
    # The following method allows dictionaries to be printed
    def __print__(self, print)
    
# Linked-list struct (implementation detail):
struct _linkedlist:
    let k
    let v
    let next: OrC(_linkedlist?, NoneC)
    
# To check if the key exists in the Dictionary
def checker(gk, gll: OrC(_linkedlist?, NoneC)):
    if gll == None:
        return gll
    else:
        if gll.k == gk:
            return gll.v
        else:
            if gll.next == None:
                return gll.next
            else:
                checker(gk, gll.next)

#Assuming the given linked list is not empty and the given key is available
#To update the linked list with a new key and value
def update(gk, gv, gll: _linkedlist?, nll: OrC(_linkedlist?, NoneC)):
    if gll.k == gk:
        nll = _linkedlist(gk, gv, nll)
    else:
        nll = _linkedlist(gll.k, gll.v, nll)
    if gll.next == None:
        return nll
    else: 
        update(gk, gv, gll.next, nll)
        
#Assuming the given linked list is not empty
#To remove a given key and associatedd value if they are available
def remove(gk, gll: _linkedlist?, nll: OrC(_linkedlist?, NoneC)):
    if gll.k != gk:
        nll = _linkedlist(gll.k, gll.v, nll)
    if gll.next == None:
        return nll
    else:
        remove(gk, gll.next, nll)

class AssociationList[K, V] (DICT):

    let _list
    let _len

    def __init__(self):
        self._list = None
        self._len = 0

    # See above.
    def __print__(self, print):
        print("#<object:AssociationList head=%p>", self._list)

    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?:
        return self._len
            
    #Is the given key mapped by the dictionary?
    def mem?(self, key: K) -> bool?:
        checker(key, self._list) != None
                    
    # Gets the value associated with the given key; calls `error` if the
    # key is not present.
    def get(self, key: K) -> V:
        let helper = checker(key, self._list)
        if helper == None:
            return error("Key does not exist")
        else:
            return helper
        
    # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC:
        if self.mem?(key):
            self._list = update(key, value, self._list, None)
        else:
            self._list = _linkedlist(key, value, self._list)
            self._len = self._len + 1
        
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC:
        if self.mem?(key):
            self._list = remove(key, self._list, None)
            self._len = self._len - 1


test 'yOu nEeD MorE tEsTs':
    let a = AssociationList()
    assert not a.mem?('hello')
    a.put('hello', 5)
    assert a.len() == 1
    assert a.mem?('hello')
    assert a.get('hello') == 5
    a.put('bye', 1)
    a.put('nice', 6)
    a.put('welp', 8)
    a.put('hi', 20)
    a.put('hey', 32)
    a.put('geez', 11)
    a.put('oh', 8)
    a.put('why', 5)
    a.put('who', 4)
    assert a.len() == 10
    a.del('hi')
    a.del('oh')
    assert a.len() == 8
    assert a.get('geez') == 11
    assert not a.mem?('hi')
    assert a.mem?('who')
    assert_error a.get('goodbye')
    
    
      
class HashTable[K, V] (DICT):
    
    let _hash
    let _size
    let _data

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._size = 0
        self._data = [None; nbuckets]
        
    # This avoids trying to print the hash function, since it's not really
    # printable and isn’t useful to see anyway:
    def __print__(self, print):
        print("#<object:HashTable  _hash=... _size=%p _data=%p>",
              self._size, self._data)

    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?:
        return self._size
        
    # Is the given key mapped by the dictionary?
    def mem?(self, key: K) -> bool?:
        let index = (self._hash(key) % self._data.len())
        checker(key, self._data[index]) != None
        
    # Gets the value associated with the given key; calls `error` if the key is not present.
    def get(self, key: K) -> V:
        let index = (self._hash(key) % self._data.len())
        let output = checker(key, self._data[index])
        if output == None:
            return error("Key does not exist")
        else:
            return output
             
    # Modifies the dictionary to associate the given key and value. If the key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC:
        let index = (self._hash(key) % self._data.len())
        if self.mem?(key):
            self._data.put(index, update(key, value, self._data[index], None))
        else:
            self._data.put(index, _linkedlist(key, value, self._data[index]))
            self._size = self._size + 1
        
        
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC:
        let index = (self._hash(key) % self._data.len())
        if self.mem?(key):
            self._data.put(index, remove(key, self._data[index], None))
            self._size = self._size + 1



# first_char_hasher(String) -> Natural
# A simple and bad hash function that just returns the ASCII code
# of the first character.
# Useful for debugging because it's easily predictable.
def first_char_hasher(s: str?) -> int?:
    if s.len() == 0:
        return 0
    else:
        return int(s[0])

test 'yOu nEeD MorE tEsTs, part 2':
    let h = HashTable(10, make_sbox_hash())
    assert not h.mem?('hello')
    h.put('hello', 5)
    assert h.len() == 1
    assert h.mem?('hello')
    assert h.get('hello') == 5
    h.put('bye', 8)
    h.put('cheese', 10)
    h.put('cry', 3)
    h.put('me', 1)
    assert h.get('me') == 1
    assert h.get('cheese') == 10
    h.del('bye')
    assert_error h.get('bye')
    assert h.mem?('cry')


struct menu:
    let dish
    let cuisine
    
def compose_menu(d: DICT!) -> DICT?:
    let f1 = menu("Shepards Pie", "Irish")
    let f2 = menu("Gouda", "American")
    let f3 = menu("Cheesecake", "American")
    let fav = d
    fav.put("Alex", f1)
    fav.put("Aalaiah", f2)
    fav.put("Victor", f3)
    return fav
    
test "AssociationList menu":
    let b = compose_menu(AssociationList())
    assert b.get("Alex") == menu("Shepards Pie", "Irish")
    assert b.get("Aalaiah").dish == "Gouda"
    assert b.get("Victor").cuisine == "American"

test "HashTable menu":
    let c = compose_menu(HashTable(5, make_sbox_hash()))
    assert c.get("Alex") == menu("Shepards Pie", "Irish")
    assert c.get("Aalaiah").dish == "Gouda"
    assert c.get("Victor").cuisine == "American"
    
time "testing":
    test 'yOu nEeD MorE tEsTs, part 3':
        let h = HashTable(1024, make_sbox_hash())
        for i in range(1024):
            let key = str(i)
            h.put(key, i)
