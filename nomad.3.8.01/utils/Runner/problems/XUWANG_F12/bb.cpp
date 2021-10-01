#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const double PI = 3.141592654;

int main ( int argc , char ** argv ) {

  double z = 1e20 ,
    g1 = 1e20 , g2 = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[2];

    for ( int i = 0 ; i < 2 ; ++i )
      in >> x[i];

    if ( !in.fail() ) {

      z = -pow(sin(2*PI*x[0]),3) * sin(2*PI*x[1]) / ( (x[0]+x[1])*pow(x[0],3));
      g1 = x[0]*x[0]-x[1]+1;
      g2 = 1-x[0]+pow(x[1]-4,2);
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << " "
       << g1 << " " << g2 << endl;

  return 0;
}
