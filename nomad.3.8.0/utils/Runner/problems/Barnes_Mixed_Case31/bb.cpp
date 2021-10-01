// $Id: Barnes.cpp 3088 2014-07-11 08:51:16Z ascrelot $
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

std::vector<double> barnes(double x1, double x2)
{
	std::vector<double> a(22);
	double a00 = 0.0;  
	double a01 = 75.196;  
	double a02 = -3.8112;  
	double a03 = 0.12694;  
	double a04 = -2.0567e-3;
	double a05 = 1.0345e-5;
	double a06 = -6.8306;
	double a07 = 0.030234;
	double a08 = -1.28134e-3;
	double a09 = 3.5256e-5;
	double a10 = -2.266e-7;
	double a11 = 0.25645;
	double a12 = -3.4604e-3;
	double a13 = 1.3514e-5;
	double a14 = -28.106;
	double a15 = -5.2375e-6;
	double a16 = -6.3e-8;
	double a17 = 7.0e-10;
	double a18 = 3.4054e-4;
	double a19 = -1.6638e-6;
	double a20 = -2.8673;
	double a21 = 0.0005;
	
	double y1 = x1 * x2;
	double y3 = x2 * x2;
	double y2 = y1 * x1;
	double y4 = x1 * x1;
	double y5 = x2 / 50.0;
	
	std::vector<double> r(4);
	
	r[0] = a01+a02*x1+a03*y4+a04*y4*x1+a05*y4*y4+a06*x2+a07*y1+a08*x1*y1+a09*y1*y4+a10*y2*y4+a11*y3+a12*x2*y3+a13*y3*y3+a14/(x2+1.0)+a15*y3*y4+a16*y1*y4*x2+a17*y1*y3*y4+a18*x1*y3+a19*y1*y3+a20*exp(a21*y1);
	r[1] = -(y1 / 700.0 - 1.0);
	r[2] = -(x2 / 5.0 - y4 / 625.0);
	r[3] = -((y5-1)*(y5-1) - (x1/500.0 - 0.11));
	return r;
}

int main(int argc, char** argv)
{
	double x1;
	double x2;
	
  if ( argc >= 2 ) {
	std::ifstream in ( argv[1] );
	  in >> x1 >> x2;
  }
  else {
	std::ifstream f_in("Parameters.in");
	f_in >> x1 >> x2;
  }
	
	std::vector<double> r = barnes(x1, x2);
	
  if ( argc >= 2 ) {
	std::cout << r[0] << std::endl;
	std::cout << r[1] << std::endl;
	std::cout << r[2] << std::endl;
	std::cout << r[3] << std::endl;
  }
  else {
	std::ofstream f_out("Responses.out");
	f_out.precision(10);
	f_out.setf(std::ios::scientific);
	f_out << r[0] << std::endl;
	f_out << r[1] << std::endl;
	f_out << r[2] << std::endl;
	f_out << r[3] << std::endl;
	
	std::ofstream f_done("Done.out");
	f_done << "success" << std::endl;
  }
	
	return 0;
}
