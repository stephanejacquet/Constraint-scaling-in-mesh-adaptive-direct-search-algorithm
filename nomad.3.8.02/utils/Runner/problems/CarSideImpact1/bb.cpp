// $Id: G07.cpp 3078 2014-07-10 16:06:20Z cbeauthi $
#include <fstream>
#include <iostream>
#include <cmath>
#include <vector>

int
main(int argc, char** argv)
{
  int dim = 11;
  std::vector<double> x(dim);
  double f, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10;
	
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
  f = 1.98+4.9*x[0]+6.67*x[1]+6.98*x[2]+4.01*x[3]+1.78*x[4]+2.73*x[6] ; 
	
  // Constraints
  g1 = 1.16-0.3717*x[1]*x[3]-0.00931*x[1]*x[9]-0.484*x[2]*x[8]+0.01343*x[5]*x[9]-1;
  g2 = 0.261-0.0159*x[0]*x[1]-0.188*x[0]*x[7]-0.019*x[1]*x[6]+0.0144*x[2]*x[4]+0.0008757*x[4]*x[9]+0.08045*x[5]*x[8]+0.00139*x[7]*x[10]+0.00001575*x[9]*x[10]-0.32;
  g3 = 0.214+0.00817*x[4]-0.131*x[0]*x[7]-0.0704*x[0]*x[8]+0.03099*x[1]*x[5]-0.018*x[1]*x[6]+0.0208*x[2]*x[7]+0.121*x[2]*x[8]-0.00364*x[4]*x[5]+0.0007715*x[4]*x[9]-0.0005354*x[5]*x[9]+0.00121*x[7]*x[10]+0.00184*x[8]*x[9]-0.02*x[1]*x[1]-0.32;  
  g4 = 0.74-0.61*x[1]-0.163*x[2]*x[7]+0.001232*x[2]*x[9]-0.166*x[6]*x[8]+0.227*x[1]*x[1]-0.32;
  g5 = 28.98+3.818*x[2]-4.2*x[0]*x[1]+0.0207*x[4]*x[9]+6.63*x[5]*x[8]-7.7*x[6]*x[7]+0.32*x[8]*x[9]-32;
  g6 = 33.86+2.95*x[2]+0.1792*x[9]-5.057*x[0]*x[1]-11*x[1]*x[7]-0.0215*x[4]*x[9]-9.98*x[6]*x[7]+22*x[7]*x[8]-32;
  g7 = 46.36-9.9*x[1]-12.9*x[0]*x[7]+0.1107*x[2]*x[9]-32; 
  g8 = 4.72-0.5*x[3]-0.19*x[1]*x[2]-0.0122*x[3]*x[9]+0.009325*x[5]*x[9]+0.000191*x[10]*x[10]-4;  
  g9 = 10.58-0.674*x[0]*x[1]-1.95*x[1]*x[7]+0.02054*x[2]*x[9]-0.0198*x[3]*x[9]+0.028*x[5]*x[9]-9.9;  
  g10 = 16.45-0.489*x[2]*x[6]-0.843*x[4]*x[5]+0.0432*x[8]*x[9]-0.0556*x[8]*x[10]-0.000786*x[10]*x[10]-15.7;    
	
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
	fout.close();
	
	std::ofstream sout("Done.out", std::ios::out);
	sout << "success" << std::endl;
	sout.close();
  }
	
  return 0;
}

