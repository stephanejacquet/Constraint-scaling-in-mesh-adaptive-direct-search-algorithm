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
      f = pow ( y - 0.1291845091*x*x + 1.591549431*x-6, 2 )
	  + 9.602112642*cos(x)+10;
  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << f << endl;

  return 0;
}
