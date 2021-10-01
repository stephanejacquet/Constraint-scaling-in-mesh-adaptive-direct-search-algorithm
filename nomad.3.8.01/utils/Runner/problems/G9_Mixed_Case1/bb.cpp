// $Id: WeldedBeam.cpp 986 2011-08-01 11:37:17Z sainvitu $

//Ref. : Online Supplement of Constrained Optimization by Radial Basis Function Interpolation for High-Dimensional Expensive Black-Box Problems with Infeasible Initial Points
//Engineering Optimization
//Rommel G. Regis
//Appendix

#include <fstream>
#include <iostream>
#include <cmath>

int main(int argc, char** argv)
{
  double f, g1, g2, g3, g4;
  double x[7];

  if ( argc >= 2 ) {
	std::ifstream in ( argv[1] );
    for (unsigned long int i = 0 ; i < 7 ; i++ ) {
	  in >> x[i];
    }
  }
  else {
    std::ifstream fin("Parameters.in", std::ios::in);
    fin >> x[0] >> x[1] >> x[2] >> x[3] >> x[4] >> x[5] >> x[6];
    fin.close();
  }

  /* objective function */
  f = (x[0] - 10.0) * (x[0] - 10.0) + 5.0 * (x[1] - 12.0) * (x[1] - 12.0) + std::pow (x[2], 4) + 3.0 * (x[3] - 11.0) * (x[3] - 11.0) + 10.0 * std::pow (x[4], 6) + 7.0 * x[5] * x[5] + std::pow (x[6], 4) - 4.0 * x[5] * x[6] - 10.0 * x[5] - 8.0 * x[6];
  /* constraints g<=0 */
  g1 = -127.0 + 2 * x[0] * x[0] + 3.0 * std::pow (x[1], 4) + x[2] + 4.0 * x[3] * x[3] + 5.0 * x[4];
  g2 = -282.0 + 7.0 * x[0] + 3.0 * x[1] + 10.0 * x[2] * x[2] + x[3] - x[4];
  g3 = -196.0 + 23.0 * x[0] + x[1] * x[1] + 6.0 * x[5] * x[5] - 8.0 * x[6];
  g4 = 4.0 * x[0] * x[0] + x[1] * x[1] - 3.0 * x[0] * x[1] + 2.0 * x[2] * x[2] + 5.0 * x[5] - 11.0 * x[6];

  if ( argc >= 2 ) {
	std::cout << f << std::endl;
	std::cout << g1 << std::endl;
	std::cout << g2 << std::endl;
	std::cout << g3 << std::endl;
	std::cout << g4 << std::endl;
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
    fout.close();

    std::ofstream sout("Done.out", std::ios::out);
    sout << "success" << std::endl;
    sout.close();
  }
		
  return 0;
}

