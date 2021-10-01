

#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <vector>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20, c1 = 1e20, c2=1e20, c3=1.0;
  double x;
  int n=2;
  std::vector<double> y(n);

  if ( argc >= 1 ) {


    ifstream in ( argv[1] );
    for ( int i = 0 ; i < n ; i++ ) {
      in >> x;
      y[i]=x;
   
        
  }
f=sqrt(pow(y[0]-20,2)+pow(y[1]-1,2));
c1=sin(y[0])-0.1-y[1];
c2=y[1]-sin(y[0]);
if (c1<=0 && c2<=0){ c3=0.0;  } 
else {c3=1.0;}
    
    in.close();
 } 
  cout << f << " " << c3 << endl;
  return 0;
}
 


