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
        let accumlist = None
        for poi in self.poi_list:
            if poi[2] = dst_cat
    
    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.    
    def plan_route(self, src_lat:  Lat?, src_lon:  Lon?, dst_name: Name?) -> ListC[RawPos?]:
        pass
        
    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(self, src_lat:  Lat?, src_lon:  Lon?, dst_cat:  Cat?, n: nat?) -> ListC[RawPOI?]:
        pass


def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == \
        cons([0,1], None)

# 'My first plan_route test':
#   assert my_first_example().plan_route(0, 0, "Pierogi") == \
#       cons([0,0], cons([0,1], None))

#test 'My first find_nearby test':
#    assert my_first_example().find_nearby(0, 0, "food", 1) == \
#        cons([0,1, "food", "Pierogi"], None)