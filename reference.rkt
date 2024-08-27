#lang dssl2
#lang dssl2

# HW4: Graph

import cons
#import 'hw4-lib/dictionaries.rkt'
import 'Nanami_D_re_dictionaries2.rkt'
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
    #   ^ ADD YOUR FIELDS HERE
    let size
    let edges
    let vertices 

    def __init__(self, size: nat?):
        self.size = size
        self.edges = [None; size]
        self.vertices = [None; size]
 
    #   ^ YOUR CODE GOES HERE
        
        # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?:
        return self.size

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC:
        
        let available?: bool? = False
        
        #update_wed():
        #Purpose: check the given WEdge's vertices are same to the given vertices u and v, and return the WEdge again 
        #         with updating its weight if those are same
        def update_wed(wed: WEdge?) -> WEdge?:
            if wed.v == v:
                available? = True
                return WEdge(u, v, w)
            else:
                return wed
                
        #update_vertices ():
        #Purpose: to update the given vertices' adjacent list in each other
        def update_vertices (u: Vertex?, v: Vertex?) -> NoneC:
            if self.vertices[u] != None:
                self.vertices.put(u, cons(v, self.vertices[u]))
            else:
                self.vertices[u] = cons(v, None)
            if self.vertices[v] != None:
                self.vertices.put(v, cons(u, self.vertices[v]))
            else:
                self.vertices[v] = cons(u, None)
        
        if u <= v:
            if self.edges[u] != None:
                if w != None:
                    self.edges.put(u, Cons.map((lambda x: update_wed(x)), self.edges[u]))
                    if available? == False:
                        self.edges.put(u, cons(WEdge(u, v, w), self.edges[u]))
                        update_vertices (u, v)            
                else:
                    self.edges.put(u, Cons.filter((lambda y: y.v != v), self.edges[u]))
                    update_vertices (u, v)
            else:
                if w != None:
                    self.edges[u] = cons(WEdge(u, v, w), None)
                    update_vertices (u, v)
        else:
            self.set_edge(v, u, w)
            
        #if u < v: check whether v is available in self.edges[u]
        #  else: check whether v is available in self.edges[u]
        #if u/v is available:
            #if weight != None: update the edges with given weight
            #             else: remove the edges from the vlist
        #               else: add the edges with given u, v, weight 

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?:
        
        #find_edge():
        #Purpose: to find the edge between two vertices
        #        ( The edge is still un directed however the inputs are labled to & from for clairity)
        def find_edge(from: Vertex?, to: Vertex?) -> OptWeight?:
            let weight = Cons.filter((lambda x: x.v == to), self.edges[from])
            if weight == None:
                return None
            else:
                return weight.data.w
        
        if u < v:
            return find_edge(u, v)
        else:
            return find_edge(v, u)

        #if u>v: check whether u is available in self.edges[v]
        #  else: check whether v is available in self.edges[u]
        #if u/v is available: find the edge from self.vlist

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?:
        return self.vertices[v]

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?:
        let wedges = None
        for x in range(0, self.size):
            wedges = Cons.app(self.edges[x], wedges)
        return wedges

# Other methods you may need can go here.
    

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
    let result = WuGraph(6) # 6-vertex graph from the assignment
    result.set_edge(0, 1, 12)
    result.set_edge(1, 2, 31)
    result.set_edge(1, 3, 56)
    result.set_edge(2, 4, -2)
    result.set_edge(2, 5, 7)
    result.set_edge(3, 4, 9)
    result.set_edge(3, 5, 1)
    
    return result
    
#   ^ YOUR CODE GOES HERE

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods() -> CityMap?:
    let g = WuGraph(6)
    let city = HashTable(6, make_sbox_hash())
    let id = HashTable(6, make_sbox_hash())
    let citymap = CityMap(g, city, id)
    
    city.put("Bellaire", 0)
    city.put("Houston", 1)
    city.put("San Antonio", 2)
    city.put("Huntsville", 3)
    city.put("Los Angeles", 4)
    city.put("Baton Rouge", 5)
    
    id.put(0, "Bellaire")
    id.put(1, "Houston")
    id.put(2, "San Antonio")
    id.put(3, "Huntsville")
    id.put(4, "Los Angeles")
    id.put(5, "Baton Rouge")
    
    #10 edges: (Please refer the attached image for the image of graph)
    g.set_edge(0, 0, 0)   #0.Bellaire <-> 0.Belliare
    g.set_edge(0, 1, 5)   #0.Bellaire <-> 1.Houston
    g.set_edge(0, 2, 10)  #0.Bellaire <-> 2.San Antonio
    g.set_edge(0, 3, 26)  #0.Bellaire <-> 3.Huntsville
    g.set_edge(0, 4, 70)  #0.Belliare <-> 4.Los Angeles
    g.set_edge(0, 5, 40)  #0.Bellaire <-> 5.Baton Rouge
    g.set_edge(1, 3, 500) #1.Houston <-> 3.Huntsville
    g.set_edge(1, 4, 1)   #1.Houston <-> 4.Los Angeles
    g.set_edge(2, 5, 33)  #2.San Antonio <-> 5.Baton Rouge
    g.set_edge(3, 4, 7)   #3.Huntsville <-> 4.Los Angeles
    
    return citymap
    
    
   
    
#   ^ YOUR CODE GOES HERE

###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    
    let false? = (lambda b: b == False)
    let visited = [False; graph.len()]
            
    def findmin(low: nat?) -> NoneC:
        if visited[low] == False:
            visit(low)
        elif low < (visited.len() - 1):
            findmin(low + 1)
    
    def visit(ver: Vertex?):
        if visited[ver] == False and graph.get_adjacent(ver) != None: f(ver)
        let vlist = Cons.filter((lambda x: (ver < x) and (visited[x] == False)), sort_vertices(graph.get_adjacent(ver)))
        Cons.map(f, vlist)
        Cons.map((lambda y: visited.put(y, True)), cons(ver, vlist))
        if visited.filter(false?) != []:
            if vlist == None:
                findmin(0) 
            else:
                visit(vlist.data) 
    visit(start)
        
       
#   ^ YOUR CODE GOES HERE

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
        
        
test 'WuGraph class':
    let g1 = WuGraph(5)
    assert g1.get_all_edges() == None
    g1.set_edge(1, 2, 12)
    assert g1.get_all_edges() == cons(WEdge(1, 2, 12), None)
    assert g1.get_edge(1, 2) == 12
    assert g1.get_edge(2, 1) == 12
    assert g1.get_adjacent(1) == cons(2, None)
    assert g1.get_adjacent(2) == cons(1, None)
    g1.set_edge(2, 3, 10)
    assert g1.get_all_edges() == cons(WEdge(2, 3, 10), cons(WEdge(1, 2, 12), None))
    assert g1.get_edge(2, 3) == 10
    assert g1.get_edge(3, 2) == 10
    assert g1.get_adjacent(2) == cons(3, cons(1, None))
    assert g1.get_adjacent(3) == cons(2, None)
    g1.set_edge(4, 3, 5)
    assert g1.get_all_edges() == cons(WEdge(3, 4, 5), cons(WEdge(2, 3, 10), cons(WEdge(1, 2, 12), None)))
    assert g1.get_edge(4, 3) == 5
    assert g1.get_edge(3, 4) == 5
    assert g1.get_adjacent(4) == cons(3, None)
    assert g1.get_adjacent(3) == cons(4, cons(2, None))
    g1.set_edge(1, 2, 30)
    assert g1.get_all_edges() == cons(WEdge(3, 4, 5), cons(WEdge(2, 3, 10), cons(WEdge(1, 2, 30), None)))
    assert g1.get_edge(1, 2) == 30
    assert g1.get_edge(2, 1) == 30
    assert g1.get_adjacent(1) == cons(2, None)
    assert g1.get_adjacent(2) == cons(3, cons(1, None))
    assert dfs_to_list(g1, 1) == cons(1, cons(2, cons(3, cons(4, None))))
    assert dfs_to_list(g1, 2) == cons(2, cons(3, cons(4, cons(1, None))))

test 'example_graph':
    let result = example_graph()
    assert result.get_all_edges() == cons(WEdge(3, 5, 1),
                                     cons(WEdge(3, 4, 9),
                                     cons(WEdge(2, 5, 7),
                                     cons(WEdge(2, 4, -2),
                                     cons(WEdge(1, 3, 56),
                                     cons(WEdge(1, 2, 31),
                                     cons(WEdge(0, 1, 12),
                                     None)))))))                                   

test 'result of my_neck_of_the_wood()':
    let res = my_neck_of_the_woods()
    let resgraph = res.graph
    let rescity = res.city_name_to_node_id
    let resid = res.node_id_to_city_name
    assert resgraph.get_all_edges() == cons(WEdge(3, 4, 7), cons(WEdge(2, 5, 33), cons(WEdge(1, 4, 1),
                                        cons(WEdge(1, 3, 500), cons(WEdge(0, 5, 40), cons(WEdge(0, 4, 70),
                                        cons(WEdge(0, 3, 26), cons(WEdge(0, 2, 10), cons(WEdge(0, 1, 5), 
                                        cons(WEdge(0, 0, 0), None))))))))))
    assert resgraph.get_edge(0, 0) == 0
    assert resgraph.get_edge(0, 1) == 5
    assert resgraph.get_edge(0, 2) == 10
    assert resgraph.get_edge(0, 3) == 26
    assert resgraph.get_edge(0, 4) == 70
    assert resgraph.get_edge(0, 5) == 40
    assert resgraph.get_edge(1, 3) == 500
    assert resgraph.get_edge(1, 4) == 1
    assert resgraph.get_edge(2, 5) == 33
    assert resgraph.get_edge(3, 4) == 7
    
    assert resgraph.get_edge(0, 0) == 0
    assert resgraph.get_edge(1, 0) == 5
    assert resgraph.get_edge(2, 0) == 10
    assert resgraph.get_edge(3, 0) == 26
    assert resgraph.get_edge(4, 0) == 70
    assert resgraph.get_edge(5, 0) == 40
    assert resgraph.get_edge(3, 1) == 500
    assert resgraph.get_edge(4, 1) == 1
    assert resgraph.get_edge(5, 2) == 33
    assert resgraph.get_edge(4, 3) == 7
    
    assert rescity.get("Bellaire") == 0
    assert rescity.get("Houston") == 1
    assert rescity.get("San Antonio") == 2
    assert rescity.get("Huntsville") == 3
    assert rescity.get("Los Angeles") == 4
    assert rescity.get("Baton Rouge") == 5
    
    assert resgraph.get_edge(0, 0) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("Bellaire"))
    assert resgraph.get_edge(0, 1) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("Houston"))
    assert resgraph.get_edge(0, 2) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("San Antonio"))
    assert resgraph.get_edge(0, 3) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("Huntsville"))
    assert resgraph.get_edge(0, 4) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("Los Angeles"))
    assert resgraph.get_edge(0, 5) == resgraph.get_edge(rescity.get("Bellaire"), rescity.get("Baton Rouge"))
    assert resgraph.get_edge(1, 3) == resgraph.get_edge(rescity.get("Houston"), rescity.get("Huntsville"))
    assert resgraph.get_edge(1, 4) == resgraph.get_edge(rescity.get("Houston"), rescity.get("Los Angeles"))
    assert resgraph.get_edge(2, 5) == resgraph.get_edge(rescity.get("San Antonio"), rescity.get("Baton Rouge"))
    assert resgraph.get_edge(3, 4) == resgraph.get_edge(rescity.get("Huntsville"), rescity.get("Los Angeles"))
    
    assert resid.get(0) == "Bellaire"
    assert resid.get(1) == "Houston"
    assert resid.get(2) == "San Antonio"
    assert resid.get(3) == "Huntsville"
    assert resid.get(4) == "Los Angeles"
    assert resid.get(5) == "Baton Rouge"