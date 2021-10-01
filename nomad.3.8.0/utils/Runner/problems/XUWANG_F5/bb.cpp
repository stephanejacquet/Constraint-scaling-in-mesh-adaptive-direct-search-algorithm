#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20 ,
    g1 = 1e20 , g2 = 1e20 , g3 = 1e20 ,
    g4 = 1e20 , g5 = 1e20 , g6 = 1e20 ,
    g7 = 1e20 , g8 = 1e20 , g9 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[13];

    for ( int i = 0 ; i < 13 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = 5*(x[0]+x[1]+x[2]+x[3])-5*(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]+x[3]*x[3])
	-x[4]-x[5]-x[6]-x[7]-x[8]-x[9]-x[10]-x[11]-x[12];

      g1 = 2*x[0]+2*x[1]+x[9]+x[10]-10;
      g2 = 2*x[0]+2*x[2]+x[9]+x[11]-10;
      g3 = 2*x[1]+2*x[2]+x[10]+x[11]-10;
      g4 = x[9]-8*x[0];
      g5 = x[10]-8*x[1];
      g6 = x[11]-8*x[2];
      g7 = -2*x[3]-x[4]+x[9];
      g8 = -2*x[5]-x[6]+x[10];
      g9 = -8*x[2]-x[8]+x[11];
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 25 );

  cout << z << " "
       << g1 << " " << g2 << " " << g3 << " "
       << g4 << " " << g5 << " " << g6 << " "
       << g7 << " " << g8 << " " << g9 << endl;
  return 0;
}

