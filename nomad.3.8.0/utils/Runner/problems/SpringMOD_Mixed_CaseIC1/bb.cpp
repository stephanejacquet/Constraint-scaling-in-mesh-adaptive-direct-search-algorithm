// $Id: WeldedBeam.cpp 986 2011-08-01 11:37:17Z sainvitu $

#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>

int main(int argc, char** argv)
{
  double f, g1, g2, g3, g4;
  std::vector<double> x(3);
  
  std::ifstream fin("Parameters.in", std::ios::in);
  fin >> x[0] >> x[1] >> x[2];
  fin.close();
  
  // Cost function
  f = (x[2]+2)*x[0]*x[1]*x[1];
  
  // Constraints
  g1 =  (71785*x[1]*x[1]*x[1]*x[1]) - (x[0]*x[0]*x[0]*x[2]);   // <= 0
  g2 = 5108 * (4*x[0]*x[0]-x[0]*x[1])*x[1]*x[1] + (12566*x[0]-x[1])*x[1]*x[1]*x[1] - 5108*(12566*x[0]-x[1])*x[1]*x[1]*x[1]*x[1]*x[1];  // <= 0
  g3 = (x[0]*x[0]*x[2])-(140.45*x[1]);  // <= 0
  g4 = x[0]+x[1]-1.5; // <= 0
  
  std::ofstream fout("Responses.out", std::ios::out);
  fout.precision(20);
  fout.setf(std::ios::scientific);
  fout << f << std::endl;
  fout << g1 << std::endl;
  fout << g2 << std::endl;
  fout << g3 << std::endl;
  fout << g4 << std::endl;
  fout.close();
  
  std::ofstream sout("Done.out", std::ios::out);
  sout << "success" << std::endl;
  sout.close();
  
  return 0;
}

