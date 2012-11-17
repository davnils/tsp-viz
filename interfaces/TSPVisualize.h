#ifndef _TSPVISUALIZE_H_
#define _TSPVISUALIZE_H_

#include <cstdint>
#include <fstream>
#include <limits>
#include <iostream>
#include <ostream>
#include <string>

/**
 * Interface providing access to realtime eucledian R^2 TSP visualization.
 * Specifices unidirectional transmission of graph information to an external binary or file.
 */

typedef uint32_t id_t;
static const std::string API_ID = "VIZ";

class TSPVisualize
{
  public:
    /**
     * Constructs a TSPVisualize instance.
     * @param os Output stream used for undirectional commands.
     */
    TSPVisualize(std::ostream & os) : out(os), vertexCount(0)
    {
    }

    /**
     * Constructs a TSPVisualize instance belonging to a file.
     * @param file Filepath where to write all output.
     */
    TSPVisualize(const std::string & file) : outFile(file.c_str(), std::ios::out), out(outFile), vertexCount(0)
    {
    }

    /**
     * Deconstructs a TSPVisualize instance, closing any associated file handles.
     */
    ~TSPVisualize()
    {
      if(outFile.is_open())
      {
        outFile.close();
      }
    }

    /**
     * Adds a new vertex with the associated coordinates in R^2.
     * Returns the associated id used in future calls to addEdge and removeEdge.
     * @param x x-coordinate of the vertex.
     * @param y y-coordinate of the vertex.
     * @return Vertex ID to be used in future calls to the API.
     */
    id_t addVertex(const float x, const float y) throw(std::string)
    {
      if(vertexCount + 1 == std::numeric_limits<id_t>::max())
      {
        throw(std::string("Visualization: surpassed number of available vertices"));
      }
      out << API_ID << " addv " << x << " " << y << std::endl;
      vertexCount++;
    }

    /**
     * Adds a new edge to the specified vertices.
     * @param a Identifier of the first vertex, as returned by addVertex.
     * @param b Identifier of the second vertex, as returned by addVertex.
     */
    void addEdge(const id_t a, const id_t b) const throw(std::string)
    {
      if(a >= vertexCount || b >= vertexCount)
      {
        throw(std::string("Visualization: invalid vertex indentifier in addEdge"));
      }
      out << API_ID << " adde " << a << " " << b << std::endl;
    }

    /**
     * Removes an existing edge between the specified vertices.
     * @param a Identifier of the first vertex, as returned by addVertex.
     * @param b Identifier of the second vertex, as returned by addVertex.
     */
    void removeEdge(const id_t a, const id_t b) const throw(std::string)
    {
      if(a >= vertexCount || b >= vertexCount)
      {
        throw(std::string("Visualization: invalid vertex indentifier in removeEdge"));
      }
      out << API_ID << " del " << a << " " << b << std::endl;
    }

  private:
    //output stream used for unidirectional API messaging
    std::ostream & out;
    //output file used in special cases
    std::ofstream outFile;
    //number of vertices added
    id_t vertexCount;
};

#endif
