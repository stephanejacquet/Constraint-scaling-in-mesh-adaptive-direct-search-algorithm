#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=0                                 */
/*       Mystery Mixed Case6                       */
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

    double f,x1,x2;

    x1=x[0].value();
    x2=x[1].value();

    f = 2.0 + 0.01 * (x2-x1*x1) * (x2-x1*x1) + (1.0-x1) *  (1.0-x1) + 2.0 * (2.0-x2)  * (2.0-x2)  + 7.0 * sin(0.5*x1) * sin(0.7*x1*x2);

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value

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
  int cont = 0;
  int nbpoints = 6;
  int nbiter = 100;

  p.set_DIMENSION (dim);             // number of variables

  vector<bb_output_type> bbot (cont+1); // definition of
  bbot[0] = OBJ;                   // output types
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
      fich >> x0[1];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  Point x1(dim);
  x1[0]=1;
  x1[1]=-0.5;
  p.set_LOWER_BOUND ( x1 );
  x1[0]=3;
  x1[1]=5.0;
  p.set_UPPER_BOUND ( x1 );

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
