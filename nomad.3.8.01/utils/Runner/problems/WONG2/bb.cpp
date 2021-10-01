/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem Wong2                          */
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
  double f1 = x[0]*x[0]+x[1]*x[1]+x[0]*x[1]-14*x[0]-16*x[1]+pow(x[2]-10,2)+
    4*pow(x[3]-5,2)+
    pow(x[4]-3,2)+2*pow(x[5]-1,2)+5*x[6]*x[6]+7*pow(x[7]-11,2)+
    2*pow(x[8]-10,2)+pow(x[9]-7,2)+45;
  z = f1;
  double f = f1 + 10*(3*pow(x[0]-2,2)+4*pow(x[1]-3,2)+2*x[2]*x[2]-7*x[3]-120);
  if ( f > z )
    z = f;
  f = f1 + 10*(5*x[0]*x[0]+8*x[1]+pow(x[2]-6,2)-2*x[3]-40);
  if ( f > z )
    z = f;
  f = f1 + 10*(0.5*pow(x[0]-8,2)+2*pow(x[1]-4,2)+3*x[4]*x[4]-x[5]-30);
  if ( f > z )
    z = f;
  f = f1 + 10*(x[0]*x[0]+2*pow(x[1]-2,2)-2*x[0]*x[1]+14*x[4]-6*x[5]);
  if ( f > z )
    z = f;
  f = f1 + 10*(4*x[0]+5*x[1]-3*x[6]+9*x[7]-105);
  if ( f > z )
    z = f;
  f = f1 + 10*(10*x[0]-8*x[1]-17*x[6]+2*x[7]);
  if ( f > z )
    z = f;
  f = f1 + 10*(-3*x[0]+6*x[1]+12*pow(x[8]-8,2)-7*x[9]);
  if ( f > z )
    z = f;
  f = f1 + 10*(-8*x[0]+2*x[1]+5*x[8]-2*x[9]-12);
  if ( f > z )
    z = f;


  if ( z > 1e+20 )
    z = 1e+20;

  cout << z << endl;

  return 0;
}
