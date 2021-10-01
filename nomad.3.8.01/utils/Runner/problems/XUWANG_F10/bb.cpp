#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20 ,
    g1 = 1e20 , g2 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[2];

    for ( int i = 0 ; i < 2 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {
      z  =  pow ( x[0]-10 , 3 ) + pow ( x[1]-20 , 3 );
      g1 = -pow ( x[0]-5 , 2 ) - pow (x[1]-5 , 2 ) + 100;
      g2 =  pow ( x[0]-6 , 2 ) + pow ( x[1]-5 , 2 ) - 82.81;
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " "
       << g1 << " " << g2 << endl;
  return 0;
}
