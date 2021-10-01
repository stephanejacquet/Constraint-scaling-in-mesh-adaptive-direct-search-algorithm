#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int N = 4;

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

  double f = 0.0;

    f += 100.0 * pow ( pow ( x[0], 2.0) - x[1] , 2.0 )
      + pow ( x[0] - 1.0 , 2.0 )
      + 90.0 * pow ( pow(x[2],2.0)-x[3] , 2.0 )
      + pow ( 1.0-x[2] , 2.0 )
      + 10.0 * pow(x[1]+x[3]-2.0,2.0)
      + 0.1 * pow ( x[1]-x[3] ,2.0 );



  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

