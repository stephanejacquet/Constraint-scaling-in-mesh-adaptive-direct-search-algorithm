#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <math.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
using namespace std;

const bool display = false;
const double EPS = 1e-3;
double bc=1.0;

int main ( int argc , char ** argv ) {

   std::ostringstream cmd;


  // Reading input file
  //---------------------
  stringstream ss;

  if (display) std::cout << "Read input file\n";
  // unscaling: [0;100] --> [0;20000]
  ifstream in ( argv[1] );
  int      i , tag;
  double   x[6] , tmp;
  for ( i = 0 ; i < 6 ; ++i ) {
    in >> x[i];
    x[i] = x[i] * 200;
    ss << x[i] << "_";
  }
  if ( in.fail() ) {
    in.close();
    cout << "error" << endl;
    return 1;
  }
  in.close();

  // Build id
  //---------------------
  srand (time(NULL));
  ss << rand();
  ss << "_" << getpid() << "_" << getppid();
  const string id = ss.str();
  if (display) std::cout << "id : " << id << "\n";
  const string bb_input_file  = "/tmp/in_"+id;
  const string bb_output_file = "/tmp/out_"+id;

  if ( argc != 2 ) {
    cout << "error" << endl;
    return 1;
  }



  // Write input file
  //---------------------
  if (display) std::cout << "Write input file\n";
  ofstream out ( bb_input_file.c_str() );
  out << 6 << endl;
  for ( i = 0 ; i < 6 ; ++i )
    out << x[i] << endl;
  out.close();

  // Run simulation
  //---------------------
  if (display) std::cout << "Run Simulation Runlock2\n";
  cmd.str("");
  cmd.clear();
  cmd << "./RunLock " << bb_input_file << " " << bb_output_file ;//<< " " << id;
  if (display) std::cout << cmd.str() << "\n";
  system ( cmd.str().c_str() );

  // Read outputs
  //---------------------
  if (display) std::cout << "Read outputs\n";
  double f , ca , cb;
  ifstream in2 ( bb_output_file.c_str() );
  in2 >> f >> ca >> cb;
  if ( in2.fail() ) {
    in2.close();
    cout << "error" << endl;
    return 1;
  }
  in2.close();

  // Display bbo
  //---------------------
  f = fabs(f-22805);
  if ((ca-EPS<=0) && (-ca-EPS<=0) && (cb-EPS<=0) && (-cb-EPS<=0) ) {bc=0.0;} else {bc=1.0;}
  cout << f << " " << bc << endl;
  //cout << f << " " << ca-EPS << " " << -ca-EPS << " " << cb-EPS << " " << -cb-EPS << endl;

  // Delete exchange files
  //---------------------
  if (display) std::cout << "Delete exchange files\n";
  cmd.str("");
  cmd.clear();
  cmd << "rm " << bb_output_file << " " << bb_input_file << " 2> /dev/null";
  if (display) std::cout << cmd.str() << "\n";
  system ( cmd.str().c_str() );

  return 0;
}
