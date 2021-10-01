#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=3                                 */
/*       Barnes Mixed Case1                       */
/*------------------------------------------------*/
class My_Evaluator : public Evaluator {

public:

  // ctor:
  My_Evaluator  ( const Parameters & p ) :
    Evaluator ( p ) {}

  // dtor:
  ~My_Evaluator ( void ) {}

  // evaluation of a point:
  bool eval_x ( Eval_Point          & x          ,
		const NOMAD::Double & h_max      ,
		bool                & count_eval   ) const 
  {

    double x1,x2;

    switch (static_cast<int>(x[0].value())) {
    case 0:
      x1=3;
      break;
    case 1:
      x1=9;
      break;
    case 2:
      x1=26;
      break;
    case 3:
      x1=49;
      break;
    case 4:
      x1=60;
      break;
    case 5:
      x1=78;
      break;
    }
    x2=x[1].value();

    std::vector<double> a(22);
    double a00 = 0.0;  
    double a01 = 75.196;  
    double a02 = -3.8112;  
    double a03 = 0.12694;  
    double a04 = -2.0567e-3;
    double a05 = 1.0345e-5;
    double a06 = -6.8306;
    double a07 = 0.030234;
    double a08 = -1.28134e-3;
    double a09 = 3.5256e-5;
    double a10 = -2.266e-7;
    double a11 = 0.25645;
    double a12 = -3.4604e-3;
    double a13 = 1.3514e-5;
    double a14 = -28.106;
    double a15 = -5.2375e-6;
    double a16 = -6.3e-8;
    double a17 = 7.0e-10;
    double a18 = 3.4054e-4;
    double a19 = -1.6638e-6;
    double a20 = -2.8673;
    double a21 = 0.0005;
	
    double y1 = x1 * x2;
    double y3 = x2 * x2;
    double y2 = y1 * x1;
    double y4 = x1 * x1;
    double y5 = x2 / 50.0;
	
    std::vector<NOMAD::Double> r(4);
	
    r[0] = a01+a02*x1+a03*y4+a04*y4*x1+a05*y4*y4+a06*x2+a07*y1+a08*x1*y1+a09*y1*y4+a10*y2*y4+a11*y3+a12*x2*y3+a13*y3*y3+a14/(x2+1.0)+a15*y3*y4+a16*y1*y4*x2+a17*y1*y3*y4+a18*x1*y3+a19*y1*y3+a20*exp(a21*y1);
    r[1] = -(y1 / 700.0 - 1.0);
    r[2] = -(x2 / 5.0 - y4 / 625.0);
    r[3] = -((y5-1)*(y5-1) - (x1/500.0 - 0.11));

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , r[0]  ); // objective value
    x.set_bb_output  ( 1 , r[1]  ); // constraints value
    x.set_bb_output  ( 2 , r[2]  );
    x.set_bb_output  ( 3 , r[3]  ); 

    return true;       // the evaluation succeeded
  }
};

/*------------------------------------------*/
/*            NOMAD main function           */
/*------------------------------------------*/
int main ( int argc , char ** argv ) {

  // NOMAD initializations:
  begin ( argc , argv );

  // display:
  Display out ( std::cout );
  out.precision ( DISPLAY_PRECISION_STD );

  //  parameters creation:
  Parameters p ( out );

  int dim = 2;
  int cont = 3;
  int nbpoints = 6;
  int nbiter = 100;

  p.set_DIMENSION (dim);             // number of variables

  vector<bb_output_type> bbot (cont+1); // definition of
  bbot[0] = OBJ;                   // output types
  bbot[1] = PB;
  bbot[2] = PB;
  bbot[3] = PB;
  p.set_BB_OUTPUT_TYPE ( bbot );
  
  Point x0(dim); // starting points
  string temp;
  int temp2;
  ifstream fich("DOE.plan");
  for (int k=0; k<7; k++)
    {
      getline(fich,temp);
    }
  for (int k=0; k<nbpoints; k++)
    {
      fich >> temp2 >> temp;
      fich >> x0[0]; 
      switch (static_cast<int>(x0[0].value())) {
      case 3:
	x0[0]=0;
	break;
      case 9:
	x0[0]=1;
	break;
      case 26:
	x0[0]=2;
	break;
      case 49:
	x0[0]=3;
	break;
      case 60:
	x0[0]=4;
	break;
      case 78:
	x0[0]=5;
	break;
      }
      fich >> x0[1];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  p.set_LOWER_BOUND ( Point ( dim , 0.0 ) );
  x0[0]=5.0;
  x0[1]=60.0;
  p.set_UPPER_BOUND ( x0 );

  p.set_BB_INPUT_TYPE (0 , INTEGER);

  p.set_MAX_BB_EVAL ( nbpoints+nbiter );

  p.set_DIRECTION_TYPE(NOMAD::ORTHO_2N);

 p.set_OPPORTUNISTIC_EVAL(false);

  //p.set_SPECULATIVE_SEARCH ( false );

  //p.set_MODEL_SEARCH( NO_MODEL );
  
  p.set_DISPLAY_DEGREE ( 3 ); 
  //p.set_DISPLAY_DEGREE ( "0300" );// display only the search step

  p.set_DISPLAY_ALL_EVAL ( true );

  p.set_DISPLAY_STATS ( "bbe sol obj" );

  p.set_STATS_FILE ( "stat.txt","BBE SOL %=.10eOBJ %.10eBBO");

  p.set_ADD_SEED_TO_FILE_NAMES ( false );

  p.set_SOLUTION_FILE ("sol.txt");

  p.set_HISTORY_FILE ("hist.txt");

  // parameters validation:
  p.check();

  // custom evaluator creation:
  My_Evaluator ev ( p );

  // algorithm creation:
  Mads mads ( p , &ev );

  // algorithm execution:
  mads.run();

  Slave::stop_slaves ( out );
  end();

  return EXIT_SUCCESS;
}
