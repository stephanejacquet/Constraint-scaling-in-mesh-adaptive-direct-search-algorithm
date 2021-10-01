#include <cmath>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20;
  double x , y;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );
    in >> x >> y;
    in.close();

    f = fabs(x-y) - 1e-6 * (x+y);

  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << f << endl;

  return 0;
}
