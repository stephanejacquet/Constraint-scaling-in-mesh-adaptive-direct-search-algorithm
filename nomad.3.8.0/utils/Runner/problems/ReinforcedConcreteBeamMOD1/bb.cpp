// $Id: WeldedBeam.cpp 986 2011-08-01 11:37:17Z sainvitu $

#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>

int main(int argc, char** argv)
{
  double f, g1, g2;
  std::vector<double> x(3);
  
  std::ifstream fin("Parameters.in", std::ios::in);
  fin >> x[0] >> x[1] >> x[2];
  fin.close();
  
  // Cost function
  f = 29.4*x[0]+0.6*x[1]*x[2];
  
  // Constraints
  g1 = x[1]-4*x[2];   // <= 0
  g2 = 180*x[2]+7.375*x[0]*x[0]-x[0]*x[1]*x[2];  // <= 0
  
  std::ofstream fout("Responses.out", std::ios::out);
  fout.precision(20);
  fout.setf(std::ios::scientific);
  fout << f << std::endl;
  fout << g1 << std::endl;
  fout << g2 << std::endl;
  fout.close();
  
  std::ofstream sout("Done.out", std::ios::out);
  sout << "success" << std::endl;
  sout.close();
  
  return 0;
}

