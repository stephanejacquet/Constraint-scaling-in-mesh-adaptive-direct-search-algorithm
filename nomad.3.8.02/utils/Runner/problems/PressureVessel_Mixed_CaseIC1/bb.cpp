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
    const double g_Pi = 3.14159265358979323846;
    double x1, x2, x3, x4;
    double f, g1, g2, g3;

    std::ifstream fin("Parameters.in", std::ios::in);
    fin >> x1 >> x2 >> x3 >> x4;
    fin.close();

    // Cost function
    f = 0.6224 * 0.0625 * x1 * x3 * x4 + 1.7781 * 0.0625 * x2 * x3 * x3 + 3.1661 * 0.0625 * 0.0625 * x1 * x1 * x4 + 19.84 * 0.0625 * 0.0625 * x1 * x1 * x3;

    // Constraints
    g1 = - 0.0625 * x1 + 0.0193 * x3;   // <= 0
    g2 = - 0.0625 * x2 + 0.00954 * x3;  // <= 0
    g3 = - g_Pi * x3 * x3 * x4 - 4.0 / 3.0 * g_Pi * x3 * x3 * x3 + 1296000.0;  // <= 0

    std::ofstream fout("Responses.out", std::ios::out);
    fout.precision(20);
    fout.setf(std::ios::scientific);
    fout << f << std::endl;
    fout << g1 << std::endl;
    fout << g2 << std::endl;
    fout << g3 << std::endl;
    fout.close();

    std::ofstream sout("Done.out", std::ios::out);
    sout << "success" << std::endl;
    sout.close();
		
		return 0;
}

