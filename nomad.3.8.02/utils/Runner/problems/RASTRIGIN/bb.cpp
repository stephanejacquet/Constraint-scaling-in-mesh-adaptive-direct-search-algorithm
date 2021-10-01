#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20;

  if ( argc >= 2 ) {

    double   x , y;
    ifstream in ( argv[1] );

    in >> x >> y;
    
    in.close();

    if ( !in.fail() )
      f = 20 + x*x + y*y - 10*cos(6.283185307*x) - 10*cos(6.283185307*y);

  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << f << endl;

  return 0;
}
