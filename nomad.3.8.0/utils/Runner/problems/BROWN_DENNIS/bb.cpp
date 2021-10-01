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

 for ( i = 1 ; i <= 20 ; ++i )
 {
     double tmp = i/5.0;
     double tmp1 = x[0] + tmp*x[1] - exp(tmp);
     double tmp2 = x[2] + sin(tmp)*x[3] - cos(tmp);
     f+=pow(  tmp1*tmp1 + tmp2*tmp2  ,2.0);
 }

  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

