// $Id: Mystery.cpp 3078 2014-07-10 16:06:20Z cbeauthi $
#include <fstream>
#include <iostream>
#include"math.h"

int
main(int argc, char** argv)
{
	double x1, x2, f;
	
  if ( argc >= 2 ) {
	std::ifstream in ( argv[1] );
	in >> x1 >> x2;
  }
  else {
	std::ifstream fin("Parameters.in", std::ios::in);
	fin >> x1 >> x2;
	fin.close();
  }
	
	f = 2.0 + 0.01 * (x2-x1*x1) * (x2-x1*x1) + (1.0-x1) *  (1.0-x1) + 2.0 * (2.0-x2)  * (2.0-x2)  + 7.0 * sin(0.5*x1) * sin(0.7*x1*x2);
	
  if ( argc >= 2 ) {
	std::cout << f << std::endl;
  }
  else {
	std::ofstream fout("Responses.out", std::ios::out);
	fout.precision(20);
	fout.setf(std::ios::scientific);
	fout << f << std::endl;
	fout.close();
	
	std::ofstream sout("Done.out", std::ios::out);
	sout << "success" << std::endl;
	sout.close();
  }
	
	return 0;
}

