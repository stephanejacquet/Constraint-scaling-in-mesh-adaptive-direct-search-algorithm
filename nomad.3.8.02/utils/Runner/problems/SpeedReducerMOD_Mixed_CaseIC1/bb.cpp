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
    double x1, x2, x3, x4, x5, x6, x7;
    double f;
    double A, B, C, D, A1, A2, B1, B2;
    double g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11;

    std::ifstream fin("Parameters.in", std::ios::in);
    fin >> x1 >> x2 >> x3 >> x4 >> x5 >> x6 >> x7;
    fin.close();

    // Cost function
    A = 3.3333 * x3 * x3 + 14.9334 * x3 - 43.0934;
    B = x6 * x6 + x7 * x7;
    C = x6 * x6 * x6 + x7 * x7 * x7;
    D = x4 * x6 * x6 + x5 * x7 * x7;
    f = 0.7854 * x1 * x2 * x2 * A - 1.508 * x1 * B + 7.477 * C + 0.7854 * D;

    // Constraints
    A1 = std::sqrt(745.0 * x4 * 745.0 * x4 + 16900000.0 * x2 * x2 * x3 * x3);
    A2 = std::sqrt(745.0 * x5 * 745.0 * x5 + 157500000.0 * x2 * x2 * x3 * x3);
    g1 = 27.0 - x1 * x2 * x2 * x3;                                // <= 0
    g2 = 397.5 - x1 * x2 * x2 * x3 * x3;                          // <= 0
    g3 = 1.93 * x4 * x4 * x4 - x2 * x6 * x6 * x6 * x6 * x3;       // <= 0
    g4 = 1.93 * x5 * x5 * x5 - x2 * x7 * x7 * x7 * x7 * x3;       // <= 0
    g5 = A1 - 110.0 * x2 * x3 * x6 * x6 * x6;                     // <= 0
    g6 = A2 - 85.0 * x2 * x3 * x7 * x7 * x7;                      // <= 0
    g7 = x2 * x3 - 40.0;                                          // <= 0
    g8 = 5.0 * x2 - x1;                                           // <= 0
    g9 = x1 - 12 * x2;                                            // <= 0
    g10 = 1.9 + 1.5 * x6 - x4;                                    // <= 0
    g11 = 1.9 + 1.1 * x7 - x5;                                    // <= 0

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
		
		return 0;
}
