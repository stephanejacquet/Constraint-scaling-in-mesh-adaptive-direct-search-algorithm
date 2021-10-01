// Watson 12

#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double x[12];

  if ( argc == 2 ) {

    ifstream in ( argv[1] );

    for ( int i = 0 ; i < 12 ; i++ )
      in >> x[i];

    if ( in.fail() ) {
      cout << 1e20 << endl;
      return 1;
    }

    in.close();
  }
  else {
    cout << 1e20 << endl;
    return 1;
  }

  double v2 = pow ( x[0] , 2 ) + pow ( x[1]-x[0]*x[0]-1 , 2 );
  double bk , ck , vk;
  int j;

  for ( int k = 1 ; k <= 29 ; ++k ) {
    
    bk = 0.0;
    for ( j = 1 ; j < 12 ; ++j )
      bk += j*x[j]*pow(k/29.0,j-1.0);
    
    ck = 0.0;
    for ( j = 1 ; j <= 12 ; ++j )
      ck += x[j-1]*pow(k/29.0,j-1.0);

    vk = bk-ck*ck-1.0;

    v2 += vk*vk;
  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << v2 << endl;

  
  return 0;
}

