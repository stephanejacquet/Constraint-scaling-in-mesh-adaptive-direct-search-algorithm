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
    
    double y[15]={0.14,0.18,0.22,0.25,0.29,0.32,0.35,0.39,0.37,0.58,0.73,0.96,1.34,2.10,4.39};

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
        double ui=i+1.0;
        double vi= 16.0-i-1.0;
        double wi=min(ui,vi);
        
        f += pow ( y[i]-( x[0]+ui/(vi*x[1]+wi*x[2])) , 2.0 );
    }
    
  cout.precision(25);

  cout << f << endl;
  

  return 0;
}
