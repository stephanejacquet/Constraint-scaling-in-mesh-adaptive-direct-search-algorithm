/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-04                                  */
/*  Description : test-problem El-Attar                       */
/*                n=6                                         */
/*                m=0                                         */
/*                x0 = [ 2 2 7 0 -2 1 ]                       */
/*                ze ~ 0.5598131                              */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 6

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
  double ti , yi , fi;
  z = 0.0;
  for ( i = 1 ; i <= 51 ; i++ ) {
    ti = 0.1*(i-1);
    yi = 0.5*exp(-ti) - exp(-2*ti)  + 0.5*exp(-3*ti) +
      1.5*exp(-1.5*ti)*sin(7*ti) + exp(-2.5*ti)*sin(5*ti);

    //     cout << "yi=" << yi << " fabs="
    // 	 << fabs ( x[0] * exp(-x[1]*ti) * cos(x[2]*ti+x[3]) + x[4] * exp(-x[5]*ti) - yi )
    // 	 << " z=" << z << endl;

    z += fabs ( x[0] * exp(-x[1]*ti) * cos(x[2]*ti+x[3]) + x[4] * exp(-x[5]*ti) - yi );


    if ( z > 1e20 ) {
      z = 1e20;
      break;
    }
  }

  cout << z << endl;

  return 0;
}
