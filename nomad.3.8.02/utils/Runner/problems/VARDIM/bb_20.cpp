#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int    N  = 20;

int main ( int argc , char ** argv ) {


  double * x = new double[N];

  int i;

  if ( argc == 2 ) {

    ifstream in ( argv[1] );

    for ( i = 0 ; i < N ; ++i )
      in >> x[i];

    if ( in.fail() ) {
      cout << 1e20 << endl;
      in.close();
      delete [] x;
      return 1;
    }

    in.close();
  }

  else  {
    cout << 1e20 << endl;
    delete [] x;
    return 1;
  }

  double f   = 0.0;

  for ( i = 0 ; i < N ; ++i ) {
    f += pow ( x[i]-1 , 2 );
  }

  double fi = 0.0;
  for ( i = 1 ; i <= N ; ++i ) {
    fi += i * (x[i-1]-1);
  }
  f += pow ( fi , 2 );

  f += pow ( fi , 4 );
 
  

  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

