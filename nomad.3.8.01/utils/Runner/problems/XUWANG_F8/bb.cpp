#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20 ,
    g1 = 1e20 , g2 = 1e20 , g3 = 1e20 ,
    g4 = 1e20 , g5 = 1e20 , g6 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[5];

    for ( int i = 0 ; i < 5 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = 5.3578547*x[2]*x[2] + 0.8356891*x[0]*x[4]
	+ 37.293239*x[0] - 40792.141;

      g1 = -85.334407 - 0.0056858*x[1]*x[4] - 0.0006262*x[0]*x[3]
	+ 0.0022053*x[2]*x[4];
      g2 = 85.334407 + 0.0056858*x[1]*x[4] + 0.0006262*x[0]*x[3]
	- 0.0022053*x[2]*x[4] - 92;
      g3 = -80.51249 - 0.0071317*x[1]*x[4] - 0.0029955*x[0]*x[1]
	- 0.0021813*x[2]*x[2] + 90;
      g4 = 80.51249 + 0.0071317*x[1]*x[4] + 0.0029955*x[0]*x[1]
	+ 0.0021813*x[2]*x[2] - 110;
      g5 = -9.300961 -0.0047026*x[2]*x[4]
	- 0.0012547*x[0]*x[2] - 0.0019085*x[2]*x[3] + 20;
      g6 = 9.300961 +0.0047026*x[2]*x[4]
	+ 0.0012547*x[0]*x[2] + 0.0019085*x[2]*x[3] - 25;
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " "
       << g1 << " " << g2 << " " << g3 << " "
       << g4 << " " << g5 << " " << g6 << endl;
  return 0;
}
