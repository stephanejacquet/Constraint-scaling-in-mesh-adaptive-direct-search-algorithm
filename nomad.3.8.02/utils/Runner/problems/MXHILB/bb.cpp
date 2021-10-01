/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem MXHILB                         */
/*                n=50                                        */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 50

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
  int    j;
  double s;
  z = -1e+20;

  for ( i = 1 ; i <= 50 ; i++ ) {

    s = 0.0;
    for ( j = 1 ; j <= 50 ; j++ )
      s += x[j-1] / (i+j-1);
    s = fabs(s);
    
    if ( s > 1e+20 ) {
      cout << 1e+20 << endl;
      return 0;
    }

    if ( s > z )
      z = s;
  }

  cout << z << endl;

  return 0;
}
