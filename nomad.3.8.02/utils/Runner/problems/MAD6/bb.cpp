/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-12                                  */
/*  Description : test-problem Mad6                           */
/*                n=7 --> 5                                   */
/*                m=7                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 5
#define PI 3.14159265359

/*-----------------------------------*/
/*           main function           */
/*-----------------------------------*/
int main ( int argc, char ** argv ) {

  double g1 = 1e+20;
  double g2 = 1e+20;
  double g3 = 1e+20;
  double g4 = 1e+20;
  double g5 = 1e+20;
  double g6 = 1e+20;
  double g7 = 1e+20;
  double  z = 1e+20;

  // input read :
  // ------------
  if ( argc < 2 ) {
    cout << g1 << " "
	 << g2 << " "
	 << g3 << " "
	 << g4 << " "
	 << g5 << " "
	 << g6 << " "
	 << g7 << " "
	 <<  z << endl;
    return 1;
  }
  ifstream in ( argv[1] );
  int i;
  double x[N+2];

  // read :
  for ( i = 0 ; i < N ; i++ )
    in >> x[i];

  if ( in.fail() ) {
    cout << g1 << " "
	 << g2 << " "
	 << g3 << " "
	 << g4 << " "
	 << g5 << " "
	 << g6 << " "
	 << g7 << " "
	 <<  z << endl;
    in.close();
    return 1;
  }
  in.close();

  x[5] = 1+x[3];
  x[6] = 3.5;

  // black-boxes eval :
  // ------------------
  g1 = -x[0]        + 0.4;
  g2 =  x[0] - x[1] + 0.4;
  g3 =  x[1] - x[2] + 0.4;
  g4 =  x[2] - x[3] + 0.4;
  g5 =  x[3] - x[4] + 0.4;
  g6 =  x[4] - x[5] + 0.4;
  g7 =  x[5] - x[6] + 0.4;

//   cout << "c1: " <<  x[0]      << " >= 0.4\n"
//        << "c2: " << -x[0]+x[1] << " >= 0.4\n"
//        << "c3: " << -x[1]+x[2] << " >= 0.4\n"
//        << "c4: " << -x[2]+x[3] << " >= 0.4\n"
//        << "c5: " << -x[3]+x[4] << " >= 0.4\n"
//        << "c6: " << -x[5]+x[6] << " >= 0.4\n"
//        << "c7: " << -x[5]+x[6] << " >= 0.4\n"
//        << "c8: " << -x[3]+x[5] << "  = 1.0\n"
//        << "c9: " <<  x[6]      << "  = 3.5\n" << endl;

  int j;
  double vi , fi;
  z = -1e20;
  for ( i = 1 ; i <= 163 ; i++ ) {

    vi = ( PI * (8.5+i*0.5) ) / 180.0;

    fi = 0.0;
    for ( j = 1 ; j <= 7 ; j++ )
      fi += cos ( 2.0 * PI * x[j-1] * sin(vi) );

    fi = ( 1 + 2 * fi ) / 15.0;
    // cout << "f" << i << "=" << fi << endl;
    if ( fabs(fi) > z )
      z = fabs(fi);
  }

  cout << g1 << " "
       << g2 << " "
       << g3 << " "
       << g4 << " "
       << g5 << " "
       << g6 << " "
       << g7 << " "
       <<  z << endl;

  return 0;
}
