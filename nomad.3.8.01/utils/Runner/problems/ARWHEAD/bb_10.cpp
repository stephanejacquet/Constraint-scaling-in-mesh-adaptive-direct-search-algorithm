#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int    N  = 10;

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

  for ( i = 1 ; i < N ; ++i )
    f += pow(pow(x[i-1],2)+pow(x[N-1],2),2)-4*x[i-1]+3.0;

  

  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

