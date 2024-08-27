#lang dssl2

# HW4: Graph

import cons
import 'hw4-lib/dictionaries.rkt'
import sbox_hash

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
    let Matrix
    let Vertices

    def __init__(self, size: nat?):
        
        # Initalizes the matrix of size size
        self.Matrix = [None; size]
        for a in range(size):
            self.Matrix[a] = [None; size]
        
        
        # Initializes the vertices
        self.Vertices = None
        for a in range(size):
            self.Vertices = cons(a, self.Vertices)
            
       
        
    # Returns the number of Vertices in the graph
    def len(self) -> nat?:
        return self.Matrix.len()

    # Sets/Removes/Updates Edge
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC:
        
        #Sets the edge to be w, whether that be the same, updated, or changed
        self.Matrix [u][v] = w
        self.Matrix [v][u] = w

    # Returns the edge between the specified vertices
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?:
        return self.Matrix [u][v]

    # Gets the adjacent vertices
    def get_adjacent(self, v: Vertex?) -> VertexList?:
        let Adj_Vertices = None
        
        # Searches the matrix to get the adjacent vertices
        for a in range(self.len()):
            if self.Matrix [v][a] is not None:
                Adj_Vertices = cons(a, Adj_Vertices)
                
        return Adj_Vertices
                
    # Gets all the edges of the graph
    def get_all_edges(self) -> WEdgeList?:
        
        # At first, there are no edges recorded
        let Current_Edges = None
        
        #For every X
        for a in range(self.len()):
            
            # For every combination with x 
            for b in range(a, self.len()):
                
                # As long as there is something in the weight slot, record it
                if self.Matrix[a][b] is not None:
                    
                    Current_Edges = cons(WEdge(a, b, self.Matrix[a][b]), Current_Edges)
                    
        return Current_Edges
        
    
###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# Turns a Vector Into a List
def Vector_To_List(a):
    return Cons.from_vec(a)         
        
        
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
    let result = WuGraph(6) # 6-vertex graph from the assignment
    
    result.set_edge(0,1,12)    
    result.set_edge(1,3,56)    
    result.set_edge(1,2,31)    
    result.set_edge(3,4,9)    
    result.set_edge(3,5,1)    
    result.set_edge(2,5,7)    
    result.set_edge(2,4,-2)
    
    return result
    
test "Example_Graph_Tests":
    let A = example_graph()
    assert A.get_edge(0,1) == 12
    assert A.get_edge(3,1) == 56
    assert A.get_edge(1,2) == 31
    assert A.get_edge(4,3) == 9
    assert A.get_edge(3,5) == 1
    assert A.get_edge(2,5) == 7
    assert A.get_edge(4,2) == -2


struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods():
    let WUG = WuGraph(6)
    let city = HashTable(6, make_sbox_hash())
    let id = HashTable(6, make_sbox_hash())
    let CM = CityMap(WUG, city, id)
    
    city.put("Waterdeep", 0)
    city.put("Neverwinter", 1)
    city.put("Baldur's Gate", 2)
    city.put("The Radiant Citadel", 3)
    city.put("Menzoberranzan", 4)
    city.put("Trostenwald", 5)
    city.put("Rosohna", 6)

    
    id.put(0, "Waterdeep")
    id.put(1, "Neverwinter")
    id.put(2, "Baldur's Gate")
    id.put(3, "The Radiant Citadel")
    id.put(4, "Menzoberranzan")
    id.put(5, "Trostenwald")
    id.put(6, "Rosohna")

    
    
    #Waterdeep To Waterdeep
    WUG.set_edge(0,0,0)
    #Waterdeep To Neverwinter
    WUG.set_edge(0,1,1000) 
    #Waterdeep To Baldur's Gate
    WUG.set_edge(0,2,3000) 
    #Waterdeep To The Radiant Citadel
    WUG.set_edge(0,3,356) 
    #Waterdeep To Mezoberranzan
    WUG.set_edge(0,4,34) 
    #Waterdeep To Trostenwald
    WUG.set_edge(0,5,7) 
    #Menzoberranzan To Neverwinter
    WUG.set_edge(4,1,6000) 
    #Menzoberranzan To Baldur's Gate
    WUG.set_edge(4,2,456) 
    #Menzoberranzan To The Radiant Citadel
    WUG.set_edge(4,3,23)  
    #Menzoberranzan To Rosohna
    WUG.set_edge(4,5,56)  
    
    return CM
    
test "my_neck_of_the_woods_all_edges":
    let dnd_world = my_neck_of_the_woods()
    let dnd_graph = dnd_world.graph
    let dnd_city = dnd_world.city_name_to_node_id
    let dnd_id = dnd_world.node_id_to_city_name
    
    assert dnd_graph.get_all_edges() == cons(WEdge(4,5,56), cons(WEdge(3, 4, 23), cons(WEdge(2, 4, 456),
                                        cons(WEdge(1, 4, 6000), cons(WEdge(0, 5, 7), cons(WEdge(0, 4, 34),
                                        cons(WEdge(0, 3, 356), cons(WEdge(0, 2, 3000), cons(WEdge(0, 1, 1000), 
                                        cons(WEdge(0, 0, 0), None))))))))))
                                        
test "my_neck_of_the_woods_specific_checks":
    let dnd_world = my_neck_of_the_woods()
    let dnd_graph = dnd_world.graph
    let dnd_city = dnd_world.city_name_to_node_id
    let dnd_id = dnd_world.node_id_to_city_name
    
    
    assert dnd_graph.get_edge(1,4) == 6000
    assert dnd_city.get("Waterdeep") == 0
    assert dnd_id.get(4) == "Menzoberranzan"


###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    
    # At the start, nothings been visited
    let visited = [False; graph.len()]

    # Helper for dfs
    def dfs_help(starter):
        
        # Is the one we're starting with been visited before?
        if not visited[starter]:
            
            # If not apply "f" and set visited to true
            f(starter)
            visited[starter] = True
            
            # Onwards to the next!
            let Next = graph.get_adjacent(starter)
            
            # Loops on itself until there is nothing left
            while Next is not None:
                
                if not visited[Next.data]:
                    
                    dfs_help(Next.data)
                    
                Next = Next.next
                
                
    # If there is nothing in the graph then theres notihng to do
    if graph.len() == 0:
        pass 
        
    # Calling the helper
    dfs_help(start)
            

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

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])
        
test 'dfs_to_list_my_neck_of_the_woods':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(my_neck_of_the_woods().graph, 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])
        
test 'dfs_to_list_my_neck_of_the_woods_Starting_point_not_zero':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(my_neck_of_the_woods().graph, 5)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5]) 
        
        
test "General Graph":
    let G = WuGraph(5)
    assert G.get_all_edges() == None
    G.set_edge (0,1,30)
    assert G.get_adjacent(0) == cons(1, None)
    assert G.get_adjacent(1) == cons(0, None)
    assert G.get_all_edges() == cons(WEdge(0, 1, 30), None)
    G.set_edge (0,2,3)
    assert G.get_all_edges() == cons(WEdge(0,2,3),cons(WEdge(0, 1, 30), None))
    G.set_edge(0,2,5)
    assert G.get_all_edges() == cons(WEdge(0,2,5),cons(WEdge(0, 1, 30), None))
    assert dfs_to_list(G, 1) == cons(1, cons(0, cons(2, None)))
    assert dfs_to_list(G, 2) == cons(2, cons(0, cons(1, None)))

    
test "Basic_Tests":
   
    let wu = WuGraph(4)
    wu.set_edge(0, 1, 10)
    wu.set_edge(1, 2, 20)
    wu.set_edge(2, 3, 30)
    assert wu.get_edge(0, 1) == 10
    assert wu.get_edge(1, 0) == 10
    assert wu.get_edge(1, 2) == 20
    assert wu.get_edge(2, 1) == 20
    assert wu.get_edge(2, 3) == 30
    assert wu.get_edge(3, 2) == 30
    assert wu.get_edge(0, 2) == None
    
    let expected_edges = Vector_To_List([WEdge(0, 1, 10), WEdge(1, 2, 20), WEdge(2, 3, 30)])
    let actual_edges = sort_edges(wu.get_all_edges())
    assert actual_edges == expected_edges
