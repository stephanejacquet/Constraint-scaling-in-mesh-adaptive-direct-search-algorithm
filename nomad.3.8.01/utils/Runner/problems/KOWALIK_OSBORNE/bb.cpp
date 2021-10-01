#include <cmath>
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

const int N = 4;

int main ( int argc , char ** argv ) {


  double * x = new double[N];

  int i;

  if ( argc == 2 ) {

    ifstream in ( argv[1] );

    for ( i = 0 ; i < N ; ++i )
      in >> x[i];

    if ( in.fail() ) {
      cout << 1e20 << endl;
      in.close();
      delete [] x;
      return 1;
    }

    in.close();
  }

  else  {
    cout << 1e20 << endl;
    delete [] x;
    return 1;
  }

  double f = 0.0;
    const double u[] = { 4 , 2 , 1 , 0.5 , 0.25 , 0.167 , 0.125 ,
        0.1 , 0.0833 , 0.0714 , 0.0625 };
    
    
    const double y[] = { 0.1957 , 0.1947 , 0.1735 , 0.1600 ,
        0.0844 , 0.0627 , 0.0456 , 0.0342 ,
        0.0323 , 0.0235 , 0.0246 };



 for ( i = 0 ; i < 11 ; ++i )
 {
     f+=pow( y[i]-x[0]*(u[i]*u[i]+u[i]*x[1])/(u[i]*u[i]+u[i]*x[2]+x[3])  ,2.0);
 }



  cout.precision(15);

  cout << f << endl;
  
  delete [] x;

  return 0;
}

