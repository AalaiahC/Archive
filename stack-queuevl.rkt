#lang dssl2

# HW2: Stacks and Queues

import ring_buffer

interface STACK[T]:
    def push(self, element: T) -> NoneC
    def pop(self) -> T
    def empty?(self) -> bool?

# Defined in the `ring_buffer` library; copied here for reference.
# Do not uncomment! or you'll get errors.
# interface QUEUE[T]:
#     def enqueue(self, element: T) -> NoneC
#     def dequeue(self) -> T
#     def empty?(self) -> bool?

# Linked-list node struct (implementation detail):
struct _cons:
    let data
    let next: OrC(_cons?, NoneC)

###
### ListStack
###

class ListStack[T] (STACK):

    let stack

    # Constructs an empty ListStack.
    def __init__ (self):
        self.stack = None

    def push(self, element: T):
        self.stack = _cons(element, self.stack)
        
    def empty?(self):
        if self.stack == None:
            return True
        else:
            return False
        
    def pop(self):
        if self.empty?() == True:
            return error("Try again... nothing is here...")
        else:
            let s = self.stack.data
            self.stack = self.stack.next
            return s

test "woefully insufficient":
    let s = ListStack()
    assert s.empty?() == True
    s.push(2)
    assert s.pop() == 2
    s.push(4)
    s.push(7)
    s.push(3)
    assert s.pop() == 3
    assert s.empty?() == False

###
### ListQueue
###

class ListQueue[T] (QUEUE):

    let Q

    # Constructs an empty ListQueue.
    def __init__ (self):
        self.Q = None
        
    def empty?(self):
        if self.Q == None:
            return True
        else:
            return False

    def enqueue(self, element: T):
        if self.empty?() == True:
            self.Q = _cons(element, self.Q)
        else:
            if self.Q.next == None:
                self.Q =  _cons(self.Q.data, _cons(element, None))
            else:
                self.Q = _cons(self.Q.data, _cons(self.Q.next, _cons(element, None)))
                
    def dequeue(self):
        if self.empty?() == True:
            return error("Umm.. What are we dequeuing exactly?")
        else:
            let q = self.Q.data
            self.Q = self.Q.next
            return q

test "woefully insufficient, part 2":
    let q = ListQueue()
    assert q.empty?() == True
    q.enqueue(2)
    q.enqueue(3)
    q.enqueue(7)
    q.enqueue(1)
    q.enqueue(9)
    q.enqueue(5)
    assert q.dequeue() == 2
    assert q.empty?() == False


###
### Playlists
###

struct song:
    let title: str?
    let artist: str?
    let album: str?
    
let WH = song("Whoa", "Snoh Aalegra", "Ugh, those feels again")
let CH = song("Cheers", "Rihanna", "Loud")
let KB = song("Kill Bill", "SZA", "SOS")
let MT = song("Money Trees", "Kendrick Lamar", "good kid, m.A.A.d city")
let ST = song("Stir It Up", "Bob Marley", "Legend")

# Enqueue five songs of your choice to the given queue, then return the first
# song that should play.
def fill_playlist (q: QUEUE!):
    let list = [WH, CH, KB, MT, ST]
    for i in range(len(list)):
        q.enqueue(list[i])
    return q.dequeue()
        
test "ListQueue playlist":
    let S = ListQueue()
    assert fill_playlist(S) == song {title: 'Whoa', artist: 'Snoh Aalegra', album: 'Ugh, those feels again'}

# To construct a RingBuffer: RingBuffer(capacity)
test "RingBuffer playlist":
    let R = RingBuffer(5)
    let F = RingBuffer(2)
    assert fill_playlist(R) == song {title: 'Whoa', artist: 'Snoh Aalegra', album: 'Ugh, those feels again'}
    assert_error fill_playlist(F)

