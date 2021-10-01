#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

int main ( int argc , char ** argv ) {

  int    i;
  double f = 1e20;

  if ( argc >= 2 ) {

    double   x[2] , sum = 0 , prod = 1;
    ifstream in ( argv[1] );

    for ( i = 0 ; i < 2 ; ++i ) {
      in >> x[i];
      sum  += x[i]*x[i];
      prod *= cos(x[i]/sqrt(i+1.0));
    }
    
    in.close();

    if ( !in.fail() )
      f = sum/4000 - prod + 1;
  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << f << endl;

  return 0;
}
