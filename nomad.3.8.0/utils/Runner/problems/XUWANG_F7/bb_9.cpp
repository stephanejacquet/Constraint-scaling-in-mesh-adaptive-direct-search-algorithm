#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

// N-1 variables

#define N  10
#define NM1 N-1
const int C = pow ( sqrt(N) , N );

int main ( int argc , char ** argv ) {

  double z = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x[N];

    x[NM1] = 1.0;

    z = 1.0;
    for ( int i = 0 ; i < NM1 ; ++i ) {
      in >> x[i];
      x[NM1] -= x[i]*x[i];
      z *= x[i];
    }


    if ( !in.fail() && x[NM1] >= 0.0 ) {
      z *= -C * sqrt(x[NM1]);
    }
    else
      z = 1e20;

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 25 );

  cout << z << endl;

  return 0;
}
