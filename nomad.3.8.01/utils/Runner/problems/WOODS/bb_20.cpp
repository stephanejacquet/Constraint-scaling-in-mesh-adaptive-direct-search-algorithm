#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int N = 20;

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

  for ( i = 1 ; i <= N/4 ; ++i ) {

    f += 100 * pow ( pow ( x[4*i-4], 2) - x[4*i-3] , 2 )
      + pow ( x[4*i-4] - 1 , 2 )
      + 90 * pow ( pow(x[4*i-2],2)-x[4*i-1] , 2 )
      + pow ( 1-x[4*i-2] , 2 )
      + 10.1 * ( pow(x[4*i-3]-1,2)+pow(x[4*i-1]-1,2) )
      + 19.8 * (x[4*i-3]-1) * (x[4*i-1]-1);


  }

  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

