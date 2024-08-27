#lang dssl2

# HW4: Graph

import sbox_hash
import cons
import 'hw4-lib/dictionaries.rkt'


###
### REPRESENTATION
###

# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?

class WuGraph (WUGRAPH):
    
    let size
    let adjacency_matrix

    def __init__(self, size: nat?):
        
        self.size = size
        self.adjacency_matrix = [None; size]
        for i in range(size):
            self.adjacency_matrix[i] = [None; size]
        
        
    # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?:
        return len(self.adjacency_matrix)

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC:
        self.adjacency_matrix[u][v] = w
        self.adjacency_matrix[v][u] = w

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?:
        return self.adjacency_matrix[u][v]

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?:
        let avert = None
        
        for i in range(self.size):
            if self.adjacency_matrix[v][i] != None:
                avert = cons(i, avert)
        return avert

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?:
        let accum = None
        
        for i in range(self.size):
            for j in range(i+1, self.size):
                if self.adjacency_matrix[i][j] != None:
                    accum = cons(WEdge (i, j, self.adjacency_matrix[i][j]), accum)
        return accum

###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
# ASSUMPTION: There's no need to compare weights because
# the same edge can’t appear with different weights.
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)

###
### BUILDING GRAPHS
###

def example_graph() -> WuGraph?:
    let graph = WuGraph(6) # 6-vertex graph from the assignment
    graph.set_edge(0, 1, 12)
    graph.set_edge(1, 2, 31)
    graph.set_edge(1, 3, 56)
    graph.set_edge(2, 4, -2)
    graph.set_edge(2, 5, 7)
    graph.set_edge(3, 4, 9)
    graph.set_edge(3, 5, 1)
    
    return graph

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods():
    let graph = WuGraph(6)
    let city = HashTable(6, make_sbox_hash())
    let city_id = HashTable(6, make_sbox_hash())
    let citymap = CityMap(graph, city, city_id)
    
    city.put("Bellaire", 0)
    city.put("Houston", 1)
    city.put("San Antonio", 2)
    city.put("Huntsville", 3)
    city.put("Los Angeles", 4)
    city.put("Baton Rouge", 5)
    
    city_id.put(0, "Bellaire")
    city_id.put(1, "Houston")
    city_id.put(2, "San Antonio")
    city_id.put(3, "Huntsville")
    city_id.put(4, "Los Angeles")
    city_id.put(5, "Baton Rouge")
    
    
    #Based on if routes to cities involve other cities
    graph.set_edge(0, 1, 9)    #Bellaire - Houston
    graph.set_edge(0, 4, 267)  #Bellaire - Los Angeles
    graph.set_edge(1, 2, 196)  #Houston - San Antonio
    graph.set_edge(1, 3, 70)   #Houston - Huntsville
    graph.set_edge(1, 5, 268)  #Houston - Baton Rouge
    graph.set_edge(2, 3, 257)  #San Antonio - Huntsville
    graph.set_edge(2, 4, 94)   #San Antonio - Los Angeles
    graph.set_edge(3, 5, 298)  #Huntsville - Baton Rouge
    
    return citymap

###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    let visited = [False; len(graph)]
    
    # Helper function for dfs
    def helper(vertex):
        
        if not visited[vertex]:
            f(vertex)
            visited[vertex] = True
            let v = graph.get_adjacent(vertex)
            
            while v != None:
                if not visited[v.data]:
                    helper(v.data)
                v = v.next
                
    if len(graph) == 0:
        pass 
        
    helper(start)

# dfs_to_list : WUGRAPH Vertex -> ListOf[Vertex]
# Performs a depth-first search starting at `start` and returns a
# list of all reachable vertices.
#
# This function uses your `dfs` function to build a list in the
# order of the search. It will pass the test below if your dfs visits
# each reachable vertex once, regardless of the order in which it calls
# `f` on them. However, you should test it more thoroughly than that
# to make sure it is calling `f` (and thus exploring the graph) in
# a correct order.
def dfs_to_list(graph: WUGRAPH!, start: Vertex?) -> VertexList?:
    let list = None
    # Add to the front when we visit a node
    dfs(graph, start, lambda new: list = cons(new, list))
    # Reverse to the get elements in visiting order.
    return Cons.rev(list)

###
### TESTING
###

## You should test your code thoroughly. Here is one test to get you started:

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])
        
        
test 'WuGraph Tests':
    let g1 = WuGraph(4)
    assert g1.get_all_edges() == None
    g1.set_edge(1, 2, 5)
    assert g1.get_all_edges() == cons(WEdge(1, 2, 5), None)
    assert g1.get_edge(1, 2) == 5
    assert g1.get_edge(2, 1) == 5
    assert g1.get_adjacent(1) == cons(2, None)
    assert g1.get_adjacent(2) == cons(1, None)
    
    g1.set_edge(2, 3, 15)
    assert g1.get_all_edges() == cons(WEdge(2, 3, 15), cons(WEdge(1, 2, 5), None))
    assert g1.get_edge(2, 3) == 15
    assert g1.get_edge(3, 2) == 15
    assert g1.get_adjacent(1) == cons(2, None)
    assert g1.get_adjacent(2) == cons(3, cons(1, None))
    
    g1.set_edge(1, 2, 25)
    assert g1.get_all_edges() == cons(WEdge(2, 3, 15), cons(WEdge(1, 2, 25), None))
    assert g1.get_edge(1, 2) == 25
    assert g1.get_edge(2, 1) == 25
    assert g1.get_adjacent(1) == cons(2, None)
    assert g1.get_adjacent(2) == cons(3, cons(1, None))
    assert dfs_to_list(g1, 1) == cons(1, cons(2, cons(3, None)))
    assert dfs_to_list(g1, 2) == cons(2, cons(3, cons(1, None)))


test 'dahood tests':
    let dahood = my_neck_of_the_woods()
    assert dahood.graph.get_all_edges() \
    == cons(WEdge(3, 5, 298), cons(WEdge(2, 4, 94), 
       cons(WEdge(2, 3, 257), cons(WEdge(1, 5, 268), 
       cons(WEdge(1, 3, 70), cons(WEdge(1, 2, 196), 
       cons(WEdge(0, 4, 267), cons(WEdge(0, 1, 9), None))))))))
   
    assert dahood.graph.get_edge(0, 1) == 9
    assert dahood.graph.get_edge(1, 0) == 9
    assert dahood.graph.get_edge(0, 4) == 267
    assert dahood.graph.get_edge(4, 0) == 267
    assert dahood.graph.get_edge(1, 2) == 196
    assert dahood.graph.get_edge(2, 1) == 196
    
    assert dahood.city_name_to_node_id.get("Bellaire") == 0
    assert dahood.city_name_to_node_id.get("Houston") == 1
    assert dahood.city_name_to_node_id.get("San Antonio") == 2
    
    assert dahood.node_id_to_city_name.get(3) == "Huntsville"
    assert dahood.node_id_to_city_name.get(4) == "Los Angeles"
    assert dahood.node_id_to_city_name.get(5) == "Baton Rouge"
    
