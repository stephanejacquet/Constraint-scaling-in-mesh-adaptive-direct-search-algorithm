/*------------------------------------------------*/
/*                   ROSENBROCK                   */
/*------------------------------------------------*/
#include <fstream>
#include <iostream>
#include <cmath>
using namespace std;

#define N   2

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
    double theta = 6.0;
    double tau =2;
    double phi = 60.0;
    
  cout.precision(25);
 if ( x[0] <=0 )
     f = theta*phi*pow(fabs(x[0]),tau) + x[1] + x[1]*x[1];
    else
        f = theta*pow(x[0],tau) + x[1] + x[1]*x[1];
  cout << f << endl;
  

  return 0;
}
