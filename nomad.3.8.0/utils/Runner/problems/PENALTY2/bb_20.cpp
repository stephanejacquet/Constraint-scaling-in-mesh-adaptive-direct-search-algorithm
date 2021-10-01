#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int    N  = 20;
const double a  = 1e-5;
const double pa = sqrt(a);

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

  double fi = x[0] - 0.2;
  double yi;
  double f  = pow ( fi , 2 );

  for ( i = 2 ; i <= N ; ++i ) {
    yi  = exp ( i / 10.0 ) + exp ( (i-1) / 10.0 );
    fi  = pa * ( exp(x[i-1]/10.0) + exp(x[i-2]/10.0) - yi );
    f  += pow ( fi , 2 );
  }

  double em01 = exp(-0.1);

  for ( i = N+1 ; i < 2*N ; ++i ) {
    fi  = pa * ( exp(x[i-N]/10.0) - em01 );
    f  += pow ( fi , 2 );
  }


  fi = 0.0;
  for ( i = 1 ; i <= N ; ++i ) {
    fi += (N-i+1)*pow(x[i-1],2);
  }
  fi -= 1.0;
  f += pow ( fi , 2 );

  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

