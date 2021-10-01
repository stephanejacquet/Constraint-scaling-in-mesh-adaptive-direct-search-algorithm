/*--------------*/
/*  problem G2  */
/*--------------*/
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <sstream>
using namespace std;

#define N 50

int main ( int argc , char ** argv ) {
    
    double z = 1e+20 , g1 = 1e+20 , g2 = 1e+20;
    
    if ( argc != 2 ) {
        cout << g1 << " " << g2 << " " << z << endl;
        return 1;
    }
    
    ifstream in ( argv[1] );
    if ( in.fail() ) {
        cout << g1 << " " << g2 << " " << z << endl;
        return 1;
    }
    
    int    i;
    double x , sum1 = 0.0 , sum2 = 0.0 , sum3 = 0.0 , prod1 = 1.0 , prod2 = 1.0;
    
    for ( i = 0 ; i < N ; i++ ) {
        
        in >> x;
        
        sum1  += pow ( cos(x) , 4 );
        sum2  += x;
        sum3  += (i+1)*x*x;
        prod1 *= pow ( cos(x) , 2 );
        if ( prod2 !=0.0 )
        {
            if ( x==0.0 )
                prod2 = 0.0;
            else
                prod2 *= x;
        }
    }
    
    if ( in.fail() || sum3==0.0 ) {
        cout << g1 << " " << g2 << " " << z << endl;
        in.close();
        return 1;
    }
    
    in.close();
    
    g1 = -prod2+0.75;
    g2 = sum2 -7.5*N;
    
    z  = - fabs ( ( sum1 - 2 * prod1 ) / sqrt(sum3) );
    
    cout << setprecision(16);
    cout << g1 << " " << g2 << " " << z << endl;
    
    return 0;
}
