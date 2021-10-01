#include <iostream>
#include <fstream>
using namespace std;

#define N  10
#define N2 100

int main ( int argc , char ** argv ) {

  if ( argc < 2 ) {
    cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
    return 1;
  }

  ifstream in ( argv[1] );
  if ( in.fail() ) {
    cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
    return 1;
  }

  long double z , g1 = 0.0 , g2 = 0.0;

  for ( int i = 0 ; i < N ; i++ ) {
    in >> z;
    g1 += (z-1)*(z-1);
    g2 += (z+1)*(z+1);
  }

  if ( in.fail() ) {
    cout << 1e+20 << " " << 1e+20 << " " << 1e+20 << endl;
    return 1;
  }

  in.close();

  cout << z << " " << g1-N2 << " " << N2-g2 << endl;

  return 0;
}
