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
    const int m=4;
    for ( i = 1 ; i <= m ; ++i )
    {
        double ti = i/100.0;
        double yi = 25.0 + pow( -50*log(ti), 2.0/3.0);
        f += pow ( exp(- pow(fabs(yi*m*i*x[1]),x[2])/x[0] ) , 2.0);
    }
    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
