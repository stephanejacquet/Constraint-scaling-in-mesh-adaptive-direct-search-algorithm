/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-05                                  */
/*  Description : test-problem Osborne2                       */
/*                n=11                                        */
/*                m=0                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 11

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
  double y[65] = {
    1.366 , 1.191 , 1.112 , 1.013 , 0.991 , 0.885 , 0.831 ,
    0.847 , 0.786 , 0.725 , 0.746 , 0.679 , 0.608 , 0.655 ,
    0.616 , 0.606 , 0.602 , 0.626 , 0.651 , 0.724 , 0.649 ,
    0.649 ,
    0.694 , 0.644 , 0.624 , 0.661 , 0.612 , 0.558 , 0.533 ,
    0.495 , 0.500 , 0.423 , 0.395 , 0.375 , 0.372 , 0.391 ,
    0.396 , 0.405 , 0.428 , 0.429 , 0.523 , 0.562 , 0.607 ,
    0.653 ,
    0.672 , 0.708 , 0.633 , 0.668 , 0.645 , 0.632 , 0.591 ,
    0.559 , 0.597 , 0.625 , 0.739 , 0.710 , 0.729 , 0.720 ,
    0.636 , 0.581 , 0.428 , 0.292 , 0.162 , 0.098 , 0.054   };

  
  double ti , fi;
  z = -1e+20;

  for ( i = 1 ; i <= 65 ; i++ ) {
    ti = 0.1*(i-1);
    fi = y[i-1]  -
      x[0]*exp(-x[4]*ti) -
      x[1]*exp(-x[5]*(ti-x[8])*(ti-x[8])) -
      x[2]*exp(-x[6]*(ti-x[9])*(ti-x[9])) -
      x[3]*exp(-x[7]*(ti-x[10])*(ti-x[10]));

    if ( fabs(fi) > z )
      z = fabs(fi);
    
    if ( z > 1e+20 || z < 0.0 ) {
      cout << 1e+20 << endl;
      return 0;
    }
    
  }


  cout << z << endl;

  return 0;
}
