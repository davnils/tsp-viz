#include <iostream>
#include "interfaces/TSPVisualize.h"
#include <vector>
#include <unistd.h>

const std::string PREFIX = "VIZ ";

int main()
{
  std::vector<std::pair<float,float>> vertices;

  vertices.push_back(std::make_pair(100.0, 110.0));
  vertices.push_back(std::make_pair(200.0, 110.0));
  vertices.push_back(std::make_pair(300.0, 110.0));
  vertices.push_back(std::make_pair(400.0, 210.0));
  vertices.push_back(std::make_pair(400.0, 310.0));
  vertices.push_back(std::make_pair(400.0, 410.0));
  vertices.push_back(std::make_pair(500.0, 410.0));
  vertices.push_back(std::make_pair(510.0, 410.0));

  TSPVisualize viz(std::cout);

  for(auto & e : vertices)
  {
    viz.addVertex(e.first, e.second);
  }

  int mode = 0;
  while(1)
  {
    mode ^= 1;
    for(int i = 0; i < vertices.size() - 1; i++)
    {
      for(int j = i + 1; j < vertices.size(); j++)
      {
        if(mode == 1)
          viz.addEdge(i, j);
        else
          viz.removeEdge(i, j);
        usleep(10000);
      }
    }

    try
    {
      viz.addEdge(123, 2);
    }
    catch(const std::string & ref)
    {
      std::cerr << "Recieved exception: " << ref << std::endl;
    }
    catch(const char * str)
    {
      std::cerr << "Recieved some other exception" << std::endl;
    }
  }

  return(0);
}
