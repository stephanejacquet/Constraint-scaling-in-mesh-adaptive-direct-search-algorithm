/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2007-11-20                                  */
/*  Description : test-problem Goffin                         */
/*                n=50                                        */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define  N 50

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

  double max = -1e+20 , sum = 0.0;
  for ( i = 0 ; i < N ; i++ ) {
    if ( x[i] > max )
      max = x[i];
    sum += x[i];

    if ( sum > 1e+20 ) {
      cout << 1e+20 << endl;
      return 0;
    }
  }

  cout << 50 * max - sum << endl;

  return 0;
}
