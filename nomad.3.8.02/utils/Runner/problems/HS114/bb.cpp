/*------------------------------------------------------------*/
/*  File        : bb.cpp                                      */
/*  Author      : Sebastien Le Digabel                        */
/*  Date        : 2008-02-12                                  */
/*  Description : test-problem HS114                          */
/*                n=9                                         */
/*                m=5                                         */
/*------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

#define N 9

/*-----------------------------------*/
/*           main function           */
/*-----------------------------------*/
int main ( int argc, char ** argv ) {

  double g1 = 1e20;
  double g2 = 1e20;
  double g3 = 1e20;
  double g4 = 1e20;
  double g5 = 1e20;
  double g6 = 1e20;

  double  z = 1e20;

  // input read :
  // ------------
  if ( argc < 2 ) {
    cout << g1 << " "
	 << g2 << " "
	 << g3 << " "
	 << g4 << " "
	 << g5 << " "
	 << g6 << " "
	 <<  z << endl;
    return 1;
  }
  ifstream in ( argv[1] );
  int i;
  double x[N];

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
	 <<  z << endl;
    in.close();
    return 1;
  }

  in.close();

  // scaling :
  double  x2 = (x[0]/100.0)*(16000-1e-5) + 1e-5;
  double  x3 = (x[1]/100.0)*(120-1e-5) + 1e-5;
  double  x4 = (x[2]/100.0)*(5000-1e-5) + 1e-5;
  double  x5 = (x[3]/100.0)*(2000-1e-5) + 1e-5;
  double  x6 = (x[4]/100.0)*8 + 85;
  double  x7 = (x[5]/100.0)*5 + 90;
  double  x8 = (x[6]/100.0)*9 + 3;
  double  x9 = (x[7]/100.0)*2.8 + 1.2;
  double x10 = (x[8]/100.0)*17 + 145;

  double  x1 = 1.22*x4-x5;

//   cout << " x1=" <<  x1 << endl;
//   cout << " x2=" <<  x2 << endl;
//   cout << " x3=" <<  x3 << endl;
//   cout << " x4=" <<  x4 << endl;
//   cout << " x5=" <<  x5 << endl;
//   cout << " x6=" <<  x6 << endl;
//   cout << " x7=" <<  x7 << endl;
//   cout << " x8=" <<  x8 << endl;
//   cout << " x9=" <<  x9 << endl;
//   cout << "x10=" << x10 << endl;


  // black-boxes eval :
  // ------------------
  double a = 0.99 , b = 0.90;

  g1 = 1e-5 - x1;
  g2 = x1 - 2000;
  g3 = 0.222 * x10 + b * x9 - 35.82;
  g4 = -0.222 * x10 - x9/b + 35.82;
  g5 = -3 * x7 + a * x10 + 133;
  g6 = 3 * x7 - x10/a - 133;

  double f1 = 5.04*x1 + 0.035*x2 + 10*x3 +3.36*x5 - 0.063*x4*x7;
  double fi = f1 + 500 * (1.12*x1 + 0.13167*x1*x8 - 0.00667*x1*x8*x8 - x4/a);
  
  z = (f1 > fi) ? f1 : fi;

  fi = f1 - 500 * (1.12*x1 + 0.13167*x1*x8 - 0.00667*x1*x8*x8 - a*x4);
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * (1.098*x8 - 0.038*x8*x8 + 0.325*x6 - x7/a + 57.425);
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * (1.098*x8 - 0.038*x8*x8 + 0.325*x6 - a*x7 + 57.425);
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * ( 98000*x3/(x4*x9+1000*x3) - x6 );
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * ( 98000*x3/(x4*x9+1000*x3) - x6 );
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * ( (x2+x5)/x1 - x8 );
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * ( (x2+x5)/x1 - x8 );
  if ( fi > z )
    z = fi;

  cout << g1 << " "
       << g2 << " "
       << g3 << " "
       << g4 << " "
       << g5 << " "
       << g6 << " "
       <<  z << endl;

  return 0;
}
