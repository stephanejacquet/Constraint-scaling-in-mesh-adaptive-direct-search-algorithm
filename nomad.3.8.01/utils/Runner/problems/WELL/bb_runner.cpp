#include <iostream>
#include <cstdlib>
using namespace std;

int main ( int argc , char ** argv ) {

  if ( argc != 2 ) {
    cerr << "error (no input file)" << endl;
    return 1;
  }

  string cmd = "cd problems/WELL; ./truth_MAC.exe " + string(argv[1]);
  system ( cmd.c_str() );

  return 0;
}
