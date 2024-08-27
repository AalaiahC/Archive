#lang dssl2

# Final project: Trip Planner

import cons
import sbox_hash
import 'project-lib/dictionaries.rkt'
import 'project-lib/graph.rkt'
import 'project-lib/stack-queue.rkt'

### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?


interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs


class TripPlanner (TRIP_PLANNER):
    let poi_list
    let seg_list
    
    def __init__(self, seg_list, poi_list):
        self.seg_list = seg_list
        self.poi_list = poi_list
        
    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(self, dst_cat: Cat?) -> ListC[RawPos?]:
        let poi_positions = None
        for poi in self.poi_list:
            if poi[2] == dst_cat:
                poi_positions = cons([poi[0], poi[1]], poi_positions)
        return Cons.rev(poi_positions)
        
    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.    
    def plan_route(self, src_lat:  Lat?, src_lon:  Lon?, dst_name: Name?) -> ListC[RawPos?]:
        
        # function to find posn of destination      
        def find_dest(name): 
            for poi in self.poi_list: 
                if poi[3] == name: 
                    return [poi[0], poi[1]]
                  
                    
        # To find the euclidean distance between two endpoints of a segment
        def seg_length(aseg):
            let distance = float((aseg[2] - aseg[0])**2 + (aseg[3] - aseg[1])**2)
            let edistance = float(distance**0.5)
            return edistance
             
                    
        # returns the road segment with the shortest euclidean distance
        def find_shortest(seg_list):
            if seg_list == None:
                return None 
            let shortest = seg_list.data
            if seg_list.next == None:
                return shortest
            else:
                let maybe = seg_list.next.data
                if seg_length(maybe) < seg_length(shortest): 
                        shortest = maybe
                find_shortest(seg_list.next)
                    
           
        # determines if an element is a member of a list of elements
        def member(element, elem_list): 
            let member? = False
            if elem_list == None:
                return member?
            elif elem_list.data == element:
                    member? = True
                    return member?
            else:
                member(element, elem_list.next)
            
        # finds all Road Segs that can be traveled to and from the given RawPos
        def possible_segs(start: RawPos?, seg_l, visited): 
            let possible = None
            for seg in seg_l: 
                if start[0] == seg[0] and start[1] == seg[1] and member([seg[2], seg[3]], visited) == False:
                    possible = cons(seg, possible)
                elif start[0] == seg[2] and start[1] == seg[3] and member([seg[0], seg[1]], visited) == False: 
                    possible = cons(seg, possible)
            return possible 
            
        # returns the RawPos from a segments to be visited next
        def find_next(aseg, seg_l) -> RawPos?: 
            if member([aseg[0], aseg[1]], seg_l): 
                return [aseg[2], aseg[3]]
            else: 
                return [aseg[0], aseg[1]]        
        
        let src_posn = [src_lat, src_lon]
        let dst_posn = find_dest(dst_name)
        let visited = cons(src_posn, None)
        let path = cons(src_posn, None)
        let curr_posn = src_posn
        
        while curr_posn[0] != dst_posn[0] or curr_posn[1] != dst_posn[1]:
            let possible_segs_list = possible_segs(curr_posn, self.seg_list, visited)
            let shortest_seg = find_shortest(possible_segs_list)
            let next_posn = find_next(shortest_seg, visited)
            
            visited = cons([shortest_seg[0], shortest_seg[1]], visited)
            visited = cons([shortest_seg[2], shortest_seg[3]], visited)
            
            curr_posn = next_posn 
            
        path = cons([dst_posn[0], dst_posn[1]], path)  
        return Cons.rev(path)         
                 
            
        
    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(self, src_lat:  Lat?, src_lon:  Lon?, dst_cat:  Cat?, n: nat?) -> ListC[RawPOI?]:
        pass
     

   
def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"],
                        [0,2, "food", "Pasta"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == \
        cons([0,1], cons([0,2], None))

test 'My first plan_route test':
   assert my_first_example().plan_route(0, 0, "Pierogi") == \
       cons([0,0], cons([0,1], None))

#test 'My first find_nearby test':
#    assert my_first_example().find_nearby(0, 0, "food", 1) == \
#        cons([0,1, "food", "Pierogi"], None)
