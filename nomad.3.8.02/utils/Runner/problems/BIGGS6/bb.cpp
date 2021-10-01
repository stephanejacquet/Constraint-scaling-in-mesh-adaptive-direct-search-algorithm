#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double x[6];

  int i;

  if ( argc == 2 ) {

    ifstream in ( argv[1] );

    for ( i = 0 ; i < 6 ; ++i )
      in >> x[i];

    if ( in.fail() ) {
      cout << 1e20 << endl;
      in.close();
      return 1;
    }

    in.close();
  }

  else  {
    cout << 1e20 << endl;
    return 1;
  }


  double f = 0.0;


  for ( i = 1 ; i <= 13 ; ++i ) {
    
    f += pow ( -exp(-0.1*i)+5*exp(-i)-3*exp(-0.4*i)
	       + x[2]*exp(-0.1*i*x[0]) - x[3]*exp(-0.1*i*x[1]) +
	       x[5]*exp(-0.1*i*x[4]) , 2 );
  }


  cout.precision(15);

  cout << f << endl;
  
  return 0;
}

