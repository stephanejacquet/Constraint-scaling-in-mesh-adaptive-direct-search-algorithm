#include <iostream>
#include <fstream>
#include <cmath>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  if ( argc < 2 ) {
    cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
    return 1;
  }

  double x1 , x2;

  if ( argc == 3 ) {
    x1 = atof(argv[1]);
    x2 = atof(argv[2]);
  }
  else {

    ifstream in ( argv[1] );
    if ( in.fail() ) {
      cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
      return 1;
    }

    in >> x1 >> x2;

    if ( in.fail() ) {
      cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
      return 1;
    }

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << sqrt( (x1-20)*(x1-20) + (x2-1)*(x2-1) ) << " "
       << sin(x1)-0.1-x2 << " " << x2-sin(x1) << endl;

  return 0;
}
