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
  
    const double y[] = { 0.844 , 0.908 , 0.932 , 0.936 , 0.925 ,
        0.908 , 0.881 , 0.850 , 0.818 , 0.784 ,
        0.751 , 0.718 , 0.685 , 0.658 , 0.628 ,
        0.603 , 0.580 , 0.558 , 0.538 , 0.522 ,
        0.506 , 0.490 , 0.478 , 0.467 , 0.457 ,
        0.448 , 0.438 , 0.431 , 0.424 , 0.420 ,
        0.414 , 0.411 , 0.406 };

    double f=0;
    for ( int i = 1 ; i <= 33 ; ++i )
    {
        double tmp = 10.0*(i-1.0);
        double tmp1 = exp(-x[3]*tmp);
        double tmp2 = exp(-x[4]*tmp);
        f+= pow( y[i-1] - (x[0] + x[1]*tmp1 + x[2]*tmp2) , 2.0) ;
    }
    
    if ( f > 1e+20 || f < 0.0 ) {
      cout << 1e+20 << endl;
      return 0;
    }
    
  cout << f << endl;

  return 0;
}
