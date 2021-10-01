

#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  double f = 1e20, c1 = 1e20, c2=1e20, c3=1.0;
  double x;
  int n=5;

  if ( argc >= 1 ) {

    c1 = -pow(n,2) ;
    c2 = pow(n,2) ;
    c3= 1.0;
    ifstream in ( argv[1] );
    for ( int i = 0 ; i < n ; i++ ) {
      in >> x;
     
      c1 += pow ( x-1 , 2 );
     
      c2 -= pow ( x+1 , 2 );
     
   
    
  
         
if (i==n-1){f = x;}
  }

if (c1<=0 && c2<=0){ c3=0.0;  } 
else {c3=1.0;}
    
    in.close();
 } 
  cout << f << " " << c3 << endl;
  return 0;
}
 


  
 

 
