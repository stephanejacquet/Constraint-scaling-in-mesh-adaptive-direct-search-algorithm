#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int N = 20;

int main ( int argc , char ** argv ) {

  double * x = new double [N];

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

  for ( i = 0 ; i < N-4 ; ++i ) {

    // Powell et Ana:

    // f += pow(pow(x[i],2) + 2*pow(x[i+1],2) + 3*pow(x[i+2],2) + 4*pow(x[i+3],2)
    // + 5*pow(x[N-1],2),2) - 4*x[i] + 3.0;

    f += pow(-4*x[i]+3,2) + +pow(pow(x[i],2)+2*pow(x[i+1],2)+3*pow(x[i+2],2)+4*pow(x[i+3],2)+5*pow(x[N-1],2),2);

  }


  cout.precision(12);

  cout << f << endl;

  delete [] x;

  return 0;
}

