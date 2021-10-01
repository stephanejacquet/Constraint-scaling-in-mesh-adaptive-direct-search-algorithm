#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20 ,
    g1 = 1e20 , g2 = 1e20 , g3 = 1e20 , g4 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[7];

    for ( int i = 0 ; i < 7 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = pow(x[0]-10,2) + 5*pow(x[1]-12,2)+pow(x[2],4)+3*pow(x[3]-11,2)
	+10*pow(x[4],6)+7*x[5]*x[5]+pow(x[6],4)-4*x[5]*x[6]-10*x[5]-8*x[6];
      g1 = -127+2*x[0]*x[0]+3*pow(x[1],4)+x[2]+4*pow(x[3],2)+5*x[4];
      g2 = -282+7*x[0]+3*x[1]+10*x[2]*x[2]+x[3]-x[4];
      g3 = -196+23*x[0]+x[1]*x[1]+6*x[5]*x[5]-8*x[6];
      g4 = 4*x[0]*x[0]+x[1]*x[1]-3*x[0]*x[1]+2*x[2]*x[2]+5*x[5]-11*x[6];
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " "
       << g1 << " " << g2 << " " << g3 << " "
       << g4 << endl;

  return 0;
}
