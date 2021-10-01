#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double z = 1e20;

  if ( argc >= 2 ) {

    ifstream in ( argv[1] );

    double x , y;

    in >> x >> y;

    if ( !in.fail() )
      z = 100*pow(y-x*x,2)+pow(1-x,2);

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << endl;
  
  return 0;
}

