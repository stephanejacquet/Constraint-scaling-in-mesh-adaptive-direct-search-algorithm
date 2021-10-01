/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem PBC1                           */
/*                n=5                                         */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 5

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

  // scaling :
  x[2] *= 10.0;
  x[4] *= 10.0;

  // black-box eval :
  // ----------------

  double ti , fi;
  z = -1e+20;

  for ( i = 1 ; i <= 30 ; i++ ) {
    ti = -1+2*(i-1)/29.0;
    fi = (x[0]+x[1]*ti+x[2]*ti*ti)/(1+x[3]*ti+x[4]*ti*ti)
         - (sqrt(1+(8*ti-1)*(8*ti-1))*atan(8*ti))/(8*ti);

//     cout << "t" << i << "=" << ti << " "
//  	 << "f" << i << "=" << fi << endl;


    if ( fabs(fi) > z )
      z = fabs(fi);
    
    if ( z > 1e+20 ) {
      cout << 1e+20 << endl;
      return 0;
    }

  }

  cout.setf(ios::fixed);
  cout.precision ( 15 );
  
  cout << z << endl;

  return 0;
}
