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
  vertices.push_back(std::make_pair(510.0, 410.0));
  vertices.push_back(std::make_pair(420.0, 410.0));
  vertices.push_back(std::make_pair(430.0, 440.0));
  vertices.push_back(std::make_pair(140.0, 450.0));
  vertices.push_back(std::make_pair(450.0, 480.0));
  vertices.push_back(std::make_pair(960.0, 410.0));
  vertices.push_back(std::make_pair(960.0, 410.0));
  vertices.push_back(std::make_pair(960.0, 310.0));
  vertices.push_back(std::make_pair(160.0, 111.0));
  vertices.push_back(std::make_pair(400.0, 994.0));
  vertices.push_back(std::make_pair(590.0, 892.0));
  vertices.push_back(std::make_pair(220.0, 794.0));
  vertices.push_back(std::make_pair(120.0, 792.0));
  vertices.push_back(std::make_pair(244.0, 791.0));
  vertices.push_back(std::make_pair(323.0, 699.0));
  vertices.push_back(std::make_pair(402.0, 490.0));
  vertices.push_back(std::make_pair(390.0, 499.0));

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
  }

  return(0);
}
