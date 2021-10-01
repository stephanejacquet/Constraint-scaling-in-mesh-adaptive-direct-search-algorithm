/*------------------------------------------------*/
/*                   ROSENBROCK                   */
/*------------------------------------------------*/
#include <fstream>
#include <iostream>
#include <cmath>
using namespace std;

#define N   3


int main ( int argc , char ** argv ) {

  double x [N];
    
    double y[15]={0.0009,0.0044,0.0175,0.0540,0.1295,0.2420,0.3521,0.3989,0.0009,0.0044,0.0175,0.0540,0.1295,0.2420,0.3521};

  int i;

  if ( argc == 2 ) {

    ifstream in ( argv[1] );

    for ( i = 0 ; i < N ; ++i )
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

    for ( i= 0 ; i<15 ; i++ )
    {
        double ti=(8.0-i-1.0)/2.0;
        
        f += pow ( x[0]*exp(-x[1]*pow(ti-x[2],2.0)/2.0)-y[i] , 2.0 );
    }
    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
