// $Id: Rosenbrock_10D.cpp 3040 2014-07-07 12:28:14Z cbeauthi $
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
	unsigned long int DIM = 10;
	
	std::vector<double> x(DIM);
	
	std::ifstream f_in("Parameters.in");
	for(unsigned long int i = 0; i < x.size(); ++i)
		f_in >> x[i];
	
	double f = rosenbrock(x);
	
	std::ofstream f_out("Responses.out");
	f_out.precision(10);
	f_out.setf(std::ios::scientific);
	f_out << f << std::endl;
	
	std::ofstream f_done("Done.out");
	f_done << "success" << std::endl;
	
	return 0;
}
