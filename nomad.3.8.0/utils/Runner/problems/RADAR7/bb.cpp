#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20;

  if ( argc >= 2 ) {

    double   g , h , x[7];
    int      i , j , k;
    ifstream in ( argv[1] );

    for ( i = 0 ; i < 7 ; ++i )
      in >> x[i];
    
    in.close();

    if ( !in.fail() ) {
 
      f = -1e20;
     
      for ( i = 1 ; i <= 7 ; ++i ) {

	g = 0.0;

	for ( j = i ; j <= 7 ; ++j ) {
	  h = 0.0;
	  for ( k = abs(2*i-j-1)+1 ; k <= j ; ++k )
	    h += x[k-1];
	  g += cos(h);
	}
	if ( g > f )
	  f = g;
	if ( -g > f )
	  f = -g;
      }
      
      for ( i = 1 ; i < 7 ; ++i ) {
	g = 0.5;
	for ( j = i+1 ; j <= 7 ; ++j ) {
	  h = 0.0;
	  for ( k = abs(2*i-j)+1 ; k <= j ; ++k )
	    h += x[k-1];
	  g += cos(h);
	}
	
	if ( g > f )
	  f = g;

	if ( -g > f )
	  f = -g;
      }
    }
  }

  cout.setf(ios::fixed);
  cout.precision(15);

  cout << f << endl;

  return 0;
}
