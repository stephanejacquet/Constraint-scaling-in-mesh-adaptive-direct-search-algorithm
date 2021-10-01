/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-12                                  */
/*  Description : test-problem Pentagon                       */
/*                n=6                                         */
/*                m=15                                        */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 6
#define PI 3.14159265359

/*-----------------------------------*/
/*           main function           */
/*-----------------------------------*/
int main ( int argc, char ** argv ) {

  double g[15] , z = 1e+20;
  g[0] = g[1] = g[2] =
    g[3] = g[4] = g[5] =
    g[6] = g[7] = g[8] =
    g[9] = g[10] = g[11] =
    g[12] = g[13] = g[14];

  // input read :
  // ------------
  if ( argc < 2 ) {
    cout << g[ 0] << " "
	 << g[ 1] << " "
	 << g[ 2] << " "
	 << g[ 3] << " "
	 << g[ 4] << " "
	 << g[ 5] << " "
	 << g[ 6] << " "
	 << g[ 7] << " "
	 << g[ 8] << " "
	 << g[ 9] << " "
	 << g[10] << " "
	 << g[11] << " "
	 << g[12] << " "
	 << g[13] << " "
	 << g[14] << " "
	 <<     z << endl;
    return 1;
  }
  ifstream in ( argv[1] );
  int i;
  double x[N];

  // read :
  for ( i = 0 ; i < N ; i++ )
    in >> x[i];

  if ( in.fail() ) {
    cout << g[ 0] << " "
	 << g[ 1] << " "
	 << g[ 2] << " "
	 << g[ 3] << " "
	 << g[ 4] << " "
	 << g[ 5] << " "
	 << g[ 6] << " "
	 << g[ 7] << " "
	 << g[ 8] << " "
	 << g[ 9] << " "
	 << g[10] << " "
	 << g[11] << " "
	 << g[12] << " "
	 << g[13] << " "
	 << g[14] << " "
	 <<     z << endl;
    in.close();
    return 1;
  }
  in.close();

  // black-boxes eval :
  // ------------------
  int j , k = 0;
  for ( i = 0 ; i <= 4 ; i+=2 )
    for ( j = 0 ; j <= 4 ; j++ )
      g[k++] = x[i]*cos(2.0*PI*j/5.0) + x[i+1]*sin(2.0*PI*j/5.0) - 1.0;
  
  double f1 = -sqrt( pow(x[0]-x[2],2) + pow(x[1]-x[3],2) );
  double f2 = -sqrt( pow(x[2]-x[4],2) + pow(x[3]-x[5],2) );
  double f3 = -sqrt( pow(x[4]-x[0],2) + pow(x[5]-x[1],2) );

  z = ( f1 > f2 ) ? f1 : f2;
  if ( f3 > z )
    z = f3;

  cout << g[ 0] << " "
       << g[ 1] << " "
       << g[ 2] << " "
       << g[ 3] << " "
       << g[ 4] << " "
       << g[ 5] << " "
       << g[ 6] << " "
       << g[ 7] << " "
       << g[ 8] << " "
       << g[ 9] << " "
       << g[10] << " "
       << g[11] << " "
       << g[12] << " "
       << g[13] << " "
       << g[14] << " "
       <<     z << endl;

  return 0;
}
