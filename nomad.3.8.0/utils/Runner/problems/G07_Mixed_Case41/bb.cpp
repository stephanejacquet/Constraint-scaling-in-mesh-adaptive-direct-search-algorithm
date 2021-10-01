// $Id: G07.cpp 3078 2014-07-10 16:06:20Z cbeauthi $
#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>

int
main(int argc, char** argv)
{
  int dim = 10;
  std::vector<double> x(dim);
	
  if ( argc >= 2 ) {
	std::ifstream in ( argv[1] );
    for (unsigned long int i = 0 ; i < x.size() ; i++ ) {
	  in >> x[i];
    }
  }
  else {
	std::ifstream fin("Parameters.in", std::ios::in);
	for (int i = 0; i < dim; i++)
	  {
		fin >> x[i];
	  } 
	fin.close();
  }
	
  // Objective function
  double f = x[0] * x[0] + x[1] * x[1] + x[0] * x[1] - 14.0 * x[0] - 16.0 * x[1] + (x[2] - 10.0) * (x[2] - 10.0) + 4.0 * (x[3] - 5.0) * (x[3] - 5.0) + (x[4] - 3.0) * (x[4] - 3.0) + 2.0 * (x[5] - 1.0) * (x[5] - 1.0) + 5.0 * x[6] * x[6] + 7.0 * (x[7] - 11.0) * (x[7] - 11.0) + 2.0 * (x[8] - 10.0) * (x[8] - 10.0) +  (x[9] - 7.0) * (x[9] - 7.0) + 45.0 ; 
	
  // Constraints
  double g1, g2, g3, g4, g5, g6, g7, g8;
	
  g1 = -105.0 + 4 * x[0] + 5.0 * x[1] - 3.0 * x[6] + 9.0 * x[7];
  g2 = 10.0 * x[0] - 8.0 * x[1] - 17.0 * x[6] + 2.0 * x[7];
  g3 = -8.0 * x[0] + 2.0 * x[1] + 5.0 * x[8] - 2.0 * x[9] - 12.0;
  g4 = 3.0 * (x[0] - 2.0) * (x[0] - 2.0) + 4.0 * (x[1] - 3.0) * (x[1] - 3.0) + 2.0 * x[2] * x[2] - 7.0 * x[3] -120.0;
  g5 = 5.0 * x[0] * x[0] + 8.0 * x[1] + (x[2] - 6.0) * (x[2] - 6.0) - 2.0 * x[3] - 40.0;
  g6 = x[0] * x[0] + 2.0 * (x[1] - 2.0) * (x[1] - 2.0) - 2.0 * x[0] *  x[1] + 14.0 * x[4] - 6.0 * x[5];
  g7 = 0.5 * (x[0] - 8.0) * (x[0] - 8.0) + 2.0 * (x[1] - 4.0) * (x[1] - 4.0) + 3.0 * x[4] * x[4] - x[5] - 30.0;
  g8 = -3.0 * x[0] + 6.0 * x[1] + 12.0 * (x[8] - 8.0) * (x[8] - 8.0) - 7.0 * x[9];  
	
  if ( argc >= 2 ) {
	std::cout << f << std::endl;
	std::cout << g1 << std::endl;
	std::cout << g2 << std::endl;
	std::cout << g3 << std::endl;
	std::cout << g4 << std::endl;
	std::cout << g5 << std::endl;
	std::cout << g6 << std::endl;
	std::cout << g7 << std::endl;
	std::cout << g8 << std::endl;
  }
  else {
	std::ofstream fout("Responses.out", std::ios::out);
	fout.precision(20);
	fout.setf(std::ios::scientific);
	fout << f << std::endl;
	fout << g1 << std::endl;
	fout << g2 << std::endl;
	fout << g3 << std::endl;
	fout << g4 << std::endl;
	fout << g5 << std::endl;
	fout << g6 << std::endl;
	fout << g7 << std::endl;
	fout << g8 << std::endl;
	fout.close();
	
	std::ofstream sout("Done.out", std::ios::out);
	sout << "success" << std::endl;
	sout.close();
  }
	
  return 0;
}

