Real time TSP tour visualization
===============================

Simple haskell program that provides real time visualization of TSP tours, with the primary target being iterative heuristics.
It compiles to a separate binary which provides a interface over some stream, typically stdout or a file handle.

Currently there is only one interface available in C++, besides talking directly with the haskell module.
This allows efficient development without getting into trouble with graphics or performance issues.

The API is built around the idea of having a set vertices, and a separate set of edges, which can belong to different tours.
Vertices are added in the beginning and then edges may be added or removed troughout the execution.


Example
-------

The following example creates two vertices and a single edge belonging to a specific tour.

    > cabal install tsp-viz
    > cat test.cc; g++ -std=c++0x test.cc -o test

    #include <iostream>
    #include "interfaces/TSPVisualize.h"
    int main()
    {
        TSPVisualize<std::string> tsp(std::cout, "Initial tour");
        tsp.addVertex(0, 0);
        tsp.addVertex(100, 100);
        tsp.addEdge(0, 1);
        return(0);
    }

    > ./test | tsp-viz



Screenshot with two tours
-------------------------
<img src="https://github.com/davnils/tsp-viz/raw/master/images/demo.png" />
