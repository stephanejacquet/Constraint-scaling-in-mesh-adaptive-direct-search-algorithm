// $Id: Rosenbrock_2D.cpp 3078 2014-07-10 16:06:20Z cbeauthi $
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

double rosenbrock(const std::vector<double>& x)
{
	unsigned long int n = x.size();
	
	double f = 0.0;
	for(unsigned long int i = 0; i < n - 1; ++i)
		f += (1.0 - x[i]) * (1.0 - x[i]) + 100.0 * (x[i+1] - x[i] * x[i]) * (x[i+1] - x[i] * x[i]);
	
	return f;
}

int main(int argc, char** argv)
{
  unsigned long int DIM = 2;
	
  std::vector<double> x(DIM);
	
  if ( argc >= 2 ) {
	std::ifstream in ( argv[1] );
    for (unsigned long int i = 0 ; i < x.size() ; i++ ) {
	  in >> x[i];
    }
  }
  else {
	std::ifstream f_in("Parameters.in");
	for(unsigned long int i = 0; i < x.size(); ++i)
	  f_in >> x[i];
  }
	
  double f = rosenbrock(x);
	
  if ( argc >= 2 ) {
	std::cout << f << std::endl;
  }
  else {
	std::ofstream f_out("Responses.out");
	f_out.precision(10);
	f_out.setf(std::ios::scientific);
	f_out << f << std::endl;
	
	std::ofstream f_done("Done.out");
	f_done << "success" << std::endl;
  }
	
  return 0;
}
