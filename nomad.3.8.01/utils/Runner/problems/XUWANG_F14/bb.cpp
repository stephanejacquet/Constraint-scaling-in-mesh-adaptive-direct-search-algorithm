#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20 , g1 = 1e20 , g2 = 1e20 , g3 = 1e20 , 
         g4 = 1e20 , g5 = 1e20 , g6 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[8];

    for ( int i = 0 ; i < 8 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = x[0]+x[1]+x[2];

      g1 = -1 + 0.0025 * (x[3]+x[5]);
      g2 = -1 + 0.0025 * (x[4]+x[6]-x[3]);
      g3 = -1 + 0.01   * (x[7]-x[4]);
      g4 = -x[0]*x[5] + 833.33252*x[3] + 100*x[0] - 83333.333;
      g5 = -x[1]*x[6] + 1250*x[4] + x[1]*x[3] - 1250*x[3];
      g6 = -x[2]*x[7] + x[2]*x[4] - 2500*x[4] + 1250000;
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " " << g1 << " " << g2 << " " << g3 << " "
       << g4 << " " << g5 << " " << g6 << endl;
  
  return 0;
}

