/*------------------------------------------------*/
/*                   ROSENBROCK                   */
/*------------------------------------------------*/
#include <fstream>
#include <iostream>
#include <cmath>
using namespace std;

#define N   3

#define PI           3.14159265358979323846

double theta(double x1, double x2)
{
    if ( x1 > 0.0 )
        return 1.0/PI*atan(x2/x1);
    else if ( x1 < 0.0 )
        return 1.0/PI*atan(x2/x1)+0.5;
    else
        return 0.25;
        
}


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

  f += pow ( 10.0*(x[2] -10*theta(x[0],x[1])) , 2.0 );
  f += pow ( 10.0*( pow( pow(x[0],2.0)+pow(x[1],2.0),0.5) -1.0) , 2.0 );
  f += pow ( x[2], 2.0 );

    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
