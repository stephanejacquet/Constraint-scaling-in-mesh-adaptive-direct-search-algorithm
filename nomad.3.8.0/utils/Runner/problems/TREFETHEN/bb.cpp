#include <iostream>
#include <cmath>
#include <fstream>
#include <iomanip>
using namespace std;

/*---------------------------------------------*/
/*                  main function              */
/*---------------------------------------------*/
int main ( int argc , char ** argv ) {

  if (argc != 2) {
    cerr << 1e20 << endl;
    return 1;
  }

  ifstream in ( argv[1] );

  double x , y;
  in >> x >> y;
  in.close();

  if ( in.fail() ) {
    cerr << 1e20 << endl;
    return 1;
  }

  double f =   (x*x+y*y)/4 + exp(sin(50*x)) - sin(10*(x+y)) + sin(60*exp(y))
             + sin(70*sin(x)) + sin(sin(80*y));

  cout.setf(ios::fixed);
  cout << setprecision(20) << f << endl;

  return 0;
}
