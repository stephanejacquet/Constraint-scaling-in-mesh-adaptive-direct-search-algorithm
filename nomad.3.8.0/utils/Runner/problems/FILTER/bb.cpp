/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2007-11-20                                  */
/*  Description : test-problem filter                         */
/*                n=9                                         */
/*                m=0                                         */
/*                x0 = [ 0 1 0 -0.15 0 -0.68 0 -0.72 0.37 ]   */
/*                z0 = 0.0138535                              */
/*                ze ~ 0.00619                                */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define  N 9
#define PI 3.14159265359L

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
  for ( i = 0 ; i < N ; i++ )
    in >> x[i];
  if ( in.fail() ) {
    cout << z << endl;
    return 1;
  }
  in.close();

  // black-box eval :
  // ----------------
  double ti , yi , vi , fi;
  z = -1e+20;
  for ( i = 1 ; i <= 41 ; i++ ) {

    // ti :
    // ----
    if ( i <= 6 )
      ti = 0.01 * (i-1);
    else if ( i <= 20 )
      ti = 0.07 + 0.03 * (i-7);
    else if ( i == 21 )
      ti = 0.5;
    else if ( i <= 35 )
      ti = 0.54 + 0.03 * (i-22);
    else
      ti = 0.95 + 0.01 * (i-36);
    

    // yi and vi :
    // -----------
    yi = fabs(1-2*ti);
    vi = PI*ti;


    // fi :
    // ----
    fi = sqrt ( ( pow( x[0]+(1+x[1])*cos(vi) ,2) + pow ( (1-x[1])*sin(vi) ,2) )
		/ ( pow(x[2]+(1+x[3])*cos(vi),2) + pow((1-x[3])*sin(vi),2)) )
      * sqrt ( ( pow(x[4]+(1+x[5])*cos(vi),2) + pow((1-x[5])*sin(vi),2) )
	       / ( pow(x[6]+(1+x[7])*cos(vi),2) + pow((1-x[7])*sin(vi),2) ) ) * x[8]
      - yi;

    if ( fabs(fi) > z )
      z = fabs(fi);
  }

  cout << z << endl;

  return 0;
}
