/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem Wong1                          */
/*                n=7                                         */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 7

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

  // read :
  for ( i = 0 ; i < N ; i++ )
    in >> x[i];

  if ( in.fail() ) {
    cout << z << endl;
    return 1;
  }
  in.close();

  // black-box eval :
  // ----------------

  double f1 = (x[0]-10)*(x[0]-10)+5*(x[1]-12)*(x[1]-12)+pow(x[2],4)+
    3*(x[3]-11)*(x[3]-11) + 10*pow(x[4],6) + 7*x[5]*x[5] +
    pow(x[6],4) - 4*x[5]*x[6] - 10*x[5] - 8*x[6];

  z = f1;

  double f = f1 + 10*(2*x[0]*x[0]+3*pow(x[1],4)+x[2]+4*x[3]*x[3]
		      +5*x[4]-127);
  
  if ( f > z )
    z = f;

  f = f1 + 10*(7*x[0]+3*x[1]+10*x[2]*x[2]+x[3]-x[4]-282);
  if ( f > z )
    z = f;

  f = f1 + 10*(23*x[0]+x[1]*x[1]+6*x[5]*x[5]-8*x[6]-196);
  if ( f > z )
    z = f;

  f = f1 + 10*(4*x[0]*x[0]+x[1]*x[1]-3*x[0]*x[1]+2*x[2]*x[2]+5*x[5]-11*x[6]);
  if ( f > z )
    z = f;
  
  if ( z > 1e+20 )
    z = 1e+20;

  cout.setf(ios::fixed);
  cout.precision ( 15 );

  cout << z << endl;

  return 0;
}
