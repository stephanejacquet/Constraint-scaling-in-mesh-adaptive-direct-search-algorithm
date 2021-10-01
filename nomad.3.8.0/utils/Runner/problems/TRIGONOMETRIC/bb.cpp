/*------------------------------------------------*/
/*                   ROSENBROCK                   */
/*------------------------------------------------*/
#include <fstream>
#include <iostream>
#include <cmath>
using namespace std;

#define N   10

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

    for ( int i=1 ; i <= N ; i++ )
    {
        double tmp = N;
        for ( int j=0 ; j < N ; j++ )
            tmp+= -cos(x[j]);
        
        tmp += i*(1.0-cos(x[i-1]) ) -sin(x[i-1]);
        
        f += tmp*tmp;
    }
    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
