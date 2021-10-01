/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem Polak2                         */
/*                n=10                                        */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 10

/*-----------------------------------*/
/*           main function           */
/*-----------------------------------*/
int main ( int argc, char ** argv ) {

  // input read :
  // ------------
  double z = 1e+20;
  if ( argc < 2 ) {
    cout << z << endl;
    return 1;
  }
  ifstream in ( argv[1] );
  if ( in.fail() ) {
    cout << z << endl;
    return 1;
  }
  int i;
  double x[N];

  // read + scaling :
  in >> x[0];
  x[0] *= 100.0;
  for ( i = 1 ; i < N ; i++ ) {
    in >> x[i];
    x[i] /= 10.0;
  }

  if ( in.fail() ) {
    cout << z << endl;
    return 1;
  }
  in.close();

  // black-box eval :
  // ----------------

  z = exp ( 1e-8*x[0]*x[0]+(x[1]+2)*(x[1]+2)+x[2]*x[2]+
	    4*x[3]*x[3]+x[4]*x[4]+x[5]*x[5]+
	    x[6]*x[6]+x[7]*x[7]+x[8]*x[8]+x[9]*x[9]);

  double f2 = exp ( 1e-8*x[0]*x[0]+(x[1]-2)*(x[1]-2)+x[2]*x[2]+
		    4*x[3]*x[3]+x[4]*x[4]+x[5]*x[5]+
		    x[6]*x[6]+x[7]*x[7]+x[8]*x[8]+x[9]*x[9]);

  if ( f2 > z )
    z = f2;

  if ( z > 1e+20 )
    z = 1e+20;

  cout.setf(ios::fixed);
  cout.precision ( 15 );

  cout << z << endl;

  return 0;
}
