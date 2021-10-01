/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem Shor                           */
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
  double A[10][5];
  A[0][0]=0.0; A[0][1]=0.0; A[0][2] = 0.0; A[0][3]=0.0; A[0][4]=0.0;
  A[1][0]=2.0; A[1][1]=1.0; A[1][2] = 1.0; A[1][3]=1.0; A[1][4]=3.0;
  A[2][0]=1.0; A[2][1]=2.0; A[2][2] = 1.0; A[2][3]=1.0; A[2][4]=2.0;
  A[3][0]=1.0; A[3][1]=4.0; A[3][2] = 1.0; A[3][3]=2.0; A[3][4]=2.0;
  A[4][0]=3.0; A[4][1]=2.0; A[4][2] = 1.0; A[4][3]=0.0; A[4][4]=1.0;
  A[5][0]=0.0; A[5][1]=2.0; A[5][2] = 1.0; A[5][3]=0.0; A[5][4]=1.0;
  A[6][0]=1.0; A[6][1]=1.0; A[6][2] = 1.0; A[6][3]=1.0; A[6][4]=1.0;
  A[7][0]=1.0; A[7][1]=0.0; A[7][2] = 1.0; A[7][3]=2.0; A[7][4]=1.0;
  A[8][0]=0.0; A[8][1]=0.0; A[8][2] = 2.0; A[8][3]=1.0; A[8][4]=0.0;
  A[9][0]=1.0; A[9][1]=1.0; A[9][2] = 2.0; A[9][3]=0.0; A[9][4]=0.0;

  double B[10] = { 1, 5, 10, 2, 4, 3, 1.7, 2.5, 6, 3.5 };
  double fi;
  int    j;
  z = -1e+20;

  for ( i = 1 ; i <= 10 ; i++ ) {
    fi = 0.0;
    for ( j = 1 ; j <= 5 ; j++ )
      fi += (x[j-1]-A[i-1][j-1])*(x[j-1]-A[i-1][j-1]);
    fi *= B[i-1];
    if ( fi > z )
      z = fi;
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
