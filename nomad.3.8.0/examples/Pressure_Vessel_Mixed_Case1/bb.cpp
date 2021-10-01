
#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <vector>
using namespace std;
int main ( int argc , char ** argv ) {

    const double g_Pi = 3.14159265358979323846;
    double x;
    double f, g1, g2, g3;
    int n=4;
  std::vector<double> y(n);

  if ( argc >= 1 ) {


    ifstream in ( argv[1] );
    for ( int i = 0 ; i < n ; i++ ) {
      in >> x;
      y[i]=x;
   
        
  }
f = 0.6224 * 0.0625 * y[0] * y[2] * y[3] + 1.7781 * 0.0625 * y[1] * y[2] * y[2] + 3.1661 * 0.0625 * 0.0625 * y[0] * y[0] * y[3] + 19.84 * 0.0625 * 0.0625 * y[0] * y[0] * y[2];
    // Constraints
    g1 = - 0.0625 * y[0] + 0.0193 * y[2];   // <= 0
    g2 = - 0.0625 * y[1] + 0.00954 * y[2];  // <= 0
    g3 = - g_Pi * y[2] * y[2] * y[3] - 4.0 / 3.0 * g_Pi * y[2] * y[2] * y[2] + 1296000.0;  // <= 0

    
    in.close();
 } 
cout << f << " "<< g1<< " "<< g2 << " "<< g3<< endl;
  return 0;
}
 
