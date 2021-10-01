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
    g7 = 1e20 , g8 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[10];

    for ( int i = 0 ; i < 10 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = x[0]*x[0]+x[1]*x[1]+x[0]*x[1]-14*x[0]-16*x[1]
	+pow(x[2]-10,2)+4*pow(x[3]-5,2)
	+pow(x[4]-3,2)+2*pow(x[5]-1,2)+5*x[6]*x[6]+7*pow(x[7]-11,2)
	+2*pow(x[8]-10,2)
	+pow(x[9]-7,2)+45;

      g1 = -105+4*x[0]+5*x[1]-3*x[6]+9*x[7];
      g2 = 10*x[0]-8*x[1]-17*x[6]+2*x[7];
      g3 = -8*x[0]+2*x[1]+5*x[8]-2*x[9]-12;
      g4 = 3*pow(x[0]-2,2)+4*pow(x[1]-3,2)+2*x[2]*x[2]-7*x[3]-120;
      g5 = 5*x[0]*x[0]+8*x[1]+pow(x[2]-6,2)-2*x[3]-40;
      g6 = x[0]*x[0]+2*pow(x[1]-2,2)-2*x[0]*x[1]+14*x[4]-x[5];
      g7 = 0.5*pow(x[0]-8,2)+2*pow(x[1]-4,2)+3*x[4]*x[4]-x[5]-30;
      g8 = -3*x[0]+6*x[1]+12*pow(x[8]-8,2)-7*x[9];
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " "
       << g1 << " " << g2 << " " << g3 << " "
       << g4 << " " << g5 << " " << g6 << " "
       << g7 << " " << g8 << endl;

  return 0;
}
