Realtime TSP tour visualization
===============================

Simple haskell program that provides realtime visualization of TSP tours, with the primary target being iterative heurestics.
It compiles to a separate binary which provides a interface over some stream, typically stdout or a file handle.

Currently there is only one interface available in C++, besides talking directly with the haskell module.
This allows efficient development without getting into trouble with graphics or performance issues.

The API is built around the idea of having a set vertices, and a separate set of edges, which can belong to different tours.
Vertices are added in the beginning and then edges may be added or removed troughout the execution.

Screenshot with two tours
-------------------------
<img src="https://github.com/davnils/tsp-viz/raw/master/images/demo.png" />
