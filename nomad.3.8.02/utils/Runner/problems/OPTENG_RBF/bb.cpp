#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20 , g1 = 1e20 , g2 = 1e20 , g3 = 1e20 , g4 = 1e20;
  double x1 , x2 , x3;

  if ( argc >= 2 ) {
    
    ifstream in ( argv[1] );
    in >> x1 >> x2 >> x3;
    in.close();

    if ( !in.fail() ) {
      
      f = (2+x3)*x1*x1*x2;

      g1 = 1 - pow(x2,3.0)*x3/(71785*pow(x1,4.0));
      g2 = (4*x2*x2-x1*x2)/(12566*(x2*pow(x1,3.0)-pow(x1,4.0)))
	+ 1/(5108*x1*x1) - 1;
      g3 = 1 - 140.45*x1/(x2*x2*x3);
      g4 = (x1+x2)/1.5 - 1;
    }


  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << f << " " << g1 << " " << g2 << " " << g3 << " " << g4 << endl;

  return 0;
}
