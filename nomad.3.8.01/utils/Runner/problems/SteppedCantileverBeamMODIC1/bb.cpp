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
  double f, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11;

  int l=100;
  int P=50000;
  int L=500;
  double deltamax=2.7;
  double sigmamax=14000;
  int E=20000000;
	
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
  f = l*(x[0]*x[1]+x[2]*x[3]+x[4]*x[5]+x[6]*x[7]+x[8]*x[9]) ; 
	
  // Constraints 
    g1 = (6*P*l)-sigmamax*x[8]*x[9]*x[9];
    g2 = (12*P*l)-sigmamax*x[6]*x[7]*x[7];
    g3 = (18*P*l)-sigmamax*x[4]*x[5]*x[5];
    g4 = (24*P*l)-sigmamax*x[2]*x[3]*x[3];
    g5 = (30*P*l)-sigmamax*x[0]*x[1]*x[1];
    g6 = ((P*l*l*l)/E)*(244*x[2]*x[3]*x[3]*x[3]*x[4]*x[5]*x[5]*x[5]*x[6]*x[7]*x[7]*x[7]*x[8]*x[9]*x[9]*x[9]+148*x[0]*x[1]*x[1]*x[1]*x[4]*x[5]*x[5]*x[5]*x[6]*x[7]*x[7]*x[7]*x[8]*x[9]*x[9]*x[9]+76*x[0]*x[1]*x[1]*x[1]*x[2]*x[3]*x[3]*x[3]*x[6]*x[7]*x[7]*x[7]*x[8]*x[9]*x[9]*x[9]+28*x[0]*x[1]*x[1]*x[1]*x[2]*x[3]*x[3]*x[3]*x[4]*x[5]*x[5]*x[5]*x[8]*x[9]*x[9]*x[9]+4*x[0]*x[1]*x[1]*x[1]*x[2]*x[3]*x[3]*x[3]*x[4]*x[5]*x[5]*x[5]*x[6]*x[7]*x[7]*x[7])-deltamax*x[0]*x[1]*x[1]*x[1]*x[2]*x[3]*x[3]*x[3]*x[4]*x[5]*x[5]*x[5]*x[6]*x[7]*x[7]*x[7]*x[8]*x[9]*x[9]*x[9];
    g7 = x[1]-20*x[0]; 
    g8 = x[3]-20*x[2];  
    g9 = x[5]-20*x[4];  
    g10 = x[7]-20*x[6];  
    g11 = x[9]-20*x[8];  
	
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
	std::cout << g9 << std::endl;
	std::cout << g10 << std::endl;
	std::cout << g11 << std::endl;
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
	fout << g9 << std::endl;
	fout << g10 << std::endl;
	fout << g11 << std::endl;
	fout.close();
	
	std::ofstream sout("Done.out", std::ios::out);
	sout << "success" << std::endl;
	sout.close();
  }
	
  return 0;
}

