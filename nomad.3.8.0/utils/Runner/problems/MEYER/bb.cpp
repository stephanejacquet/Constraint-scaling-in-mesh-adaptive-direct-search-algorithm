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
    
    const double y[] = { 34780 , 28610 , 23650 , 19630 ,
        16370 , 13720 , 11540 ,  9744 ,
        8261 ,  7030 ,  6005 ,  5147 ,
        4427 ,  3820 ,  3307 ,  2872   };

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
    
    for ( i = 0 ; i < 16 ; ++i )
    {
        double ti = 5.0*(i+1) +45.0;
        f += pow ( x[0]*exp(x[1]/(ti+x[2])) - y[i] , 2.0);
    }
    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
