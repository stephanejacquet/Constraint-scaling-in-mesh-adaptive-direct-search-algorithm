#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=7, m=4                                 */
/*       G9 Mixed Case                       */
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
    Double f;
    Double g1, g2, g3, g4;

/* objective function */
    f = (x[0] - 10.0) * (x[0] - 10.0) + 5.0 * (x[1] - 12.0) * (x[1] - 12.0) + std::pow (x[2].value(), 4) + 3.0 * (x[3] - 11.0) * (x[3] - 11.0) + 10.0 * std::pow (x[4].value(), 6) + 7.0 * x[5] * x[5] + std::pow (x[6].value(), 4) - 4.0 * x[5] * x[6] - 10.0 * x[5] - 8.0 * x[6];
  /* constraints g<=0 */
    g1 = -127.0 + 2 * x[0] * x[0] + 3.0 * std::pow (x[1].value(), 4) + x[2] + 4.0 * x[3] * x[3] + 5.0 * x[4];
  g2 = -282.0 + 7.0 * x[0] + 3.0 * x[1] + 10.0 * x[2] * x[2] + x[3] - x[4];
  g3 = -196.0 + 23.0 * x[0] + x[1] * x[1] + 6.0 * x[5] * x[5] - 8.0 * x[6];
  g4 = 4.0 * x[0] * x[0] + x[1] * x[1] - 3.0 * x[0] * x[1] + 2.0 * x[2] * x[2] + 5.0 * x[5] - 11.0 * x[6];

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); 
    x.set_bb_output  ( 2 , g2  ); 
    x.set_bb_output  ( 3 , g3  ); 
    x.set_bb_output  ( 4 , g4  ); 

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

  int dim = 7;
  int cont = 4;
  int nbpoints = 21;
  int nbiter = 100;

  p.set_DIMENSION (dim);             // number of variables

  vector<bb_output_type> bbot (cont+1); // definition of
  bbot[0] = OBJ;                   // output types
  bbot[1] = PB;                   
  bbot[2] = PB;                   
  bbot[3] = PB;
  bbot[4] = PB;                                     
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
      fich >> x0[2]; 
      fich >> x0[3];
      fich >> x0[4]; 
      fich >> x0[5];
      fich >> x0[6]; 
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  x0[0]=-10.0;
  x0[1]=-10.0;
  x0[2]=-10.0;
  x0[3]=-10.0;
  x0[4]=-10.0;
  x0[5]=-10.0;
  x0[6]=-10.0;
  p.set_LOWER_BOUND ( x0 );
  x0[0]=10.0;
  x0[1]=10.0;
  x0[2]=10.0;
  x0[3]=10.0;
  x0[4]=10.0;
  x0[5]=10.0;
  x0[6]=10.0;
  p.set_UPPER_BOUND ( x0 );

  p.set_BB_INPUT_TYPE (0 , INTEGER);
  p.set_BB_INPUT_TYPE (1 , INTEGER);
  p.set_BB_INPUT_TYPE (2 , INTEGER);

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
