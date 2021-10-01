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
      z = (pow(x-100,2)+pow(y-100,2))/4000
	- cos(x-100) * cos((y-100)/sqrt(2)) + 1;

    in.close();
  }

  cout.setf(ios::fixed);
  cout.precision ( 20 );

  cout << z << endl;
  
  return 0;
}

