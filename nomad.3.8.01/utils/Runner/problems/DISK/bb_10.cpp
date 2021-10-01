#include <iostream>
#include <iomanip>
#include <fstream>
#include <ctime>
using namespace std;


int main ( int argc , char ** argv ) {

  long double z = 1e+20;
  long double h = 1e+20;

  if ( argc >= 2 ) {
    ifstream in (argv[1]);
    if ( !in.fail() ) {
      long double x[10];
      z = 0.0;
      h = 0.0;
      for ( int i = 0 ; i < 10 ; i++ ) {
	in >> x[i];
	z += x[i];
	h += x[i]*x[i];
      }
      h -= 30.0;
      if ( in.fail() ) {
	z = 1e+20;
	h = 1e+20;
      }
    }
    in.close();
  }

  cout << h << " " << z << endl;

  return 0;
}
