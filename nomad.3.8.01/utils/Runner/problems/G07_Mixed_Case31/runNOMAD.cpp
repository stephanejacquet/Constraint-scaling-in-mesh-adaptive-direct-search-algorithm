#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=8                                */
/*       G07 Mixed Case3                          */
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
    Double f,g1,g2,g3,g4,g5,g6,g7,g8;

    vector<Double> y(10);
    for (int k=0; k<10; k++){
      y[k]=x[k];
    }
	for(int k=0; k<6; k++)
	  {
	    switch (static_cast<int>(x[k].value())) {
	    case 0:
	      y[k]=-10;
	      break;
	    case 1:
	      y[k]=-5;
	      break;
	    case 2:
	      y[k]=0;
	      break;
	    case 3:
	      y[k]=1.3;
	      break;
	    case 4:
	      y[k]=2.2;
	      break;
	    case 5:
	      y[k]=5;
	      break;
	    case 6:
	      y[k]=8.2;
	      break;
	    case 7:
	      y[k]=8.7;
	      break;
	    case 8:
	      y[k]=9.5;
	      break;
	    case 9:
	      y[k]=10;
	      break;
	    }
	  }
	
	// Objective function
	f = y[0] * y[0] + y[1] * y[1] + y[0] * y[1] - 14.0 * y[0] - 16.0 * y[1] + (y[2] - 10.0) * (y[2] - 10.0) + 4.0 * (y[3] - 5.0) * (y[3] - 5.0) + (y[4] - 3.0) * (y[4] - 3.0) + 2.0 * (y[5] - 1.0) * (y[5] - 1.0) + 5.0 * y[6] * y[6] + 7.0 * (y[7] - 11.0) * (y[7] - 11.0) + 2.0 * (y[8] - 10.0) * (y[8] - 10.0) +  (y[9] - 7.0) * (y[9] - 7.0) + 45.0 ; 
	
	// Constraints
	
	g1 = -105.0 + 4 * y[0] + 5.0 * y[1] - 3.0 * y[6] + 9.0 * y[7];
	g2 = 10.0 * y[0] - 8.0 * y[1] - 17.0 * y[6] + 2.0 * y[7];
	g3 = -8.0 * y[0] + 2.0 * y[1] + 5.0 * y[8] - 2.0 * y[9] - 12.0;
	g4 = 3.0 * (y[0] - 2.0) * (y[0] - 2.0) + 4.0 * (y[1] - 3.0) * (y[1] - 3.0) + 2.0 * y[2] * y[2] - 7.0 * y[3] -120.0;
	g5 = 5.0 * y[0] * y[0] + 8.0 * y[1] + (y[2] - 6.0) * (y[2] - 6.0) - 2.0 * y[3] - 40.0;
	g6 = y[0] * y[0] + 2.0 * (y[1] - 2.0) * (y[1] - 2.0) - 2.0 * y[0] *  y[1] + 14.0 * y[4] - 6.0 * y[5];
	g7 = 0.5 * (y[0] - 8.0) * (y[0] - 8.0) + 2.0 * (y[1] - 4.0) * (y[1] - 4.0) + 3.0 * y[4] * y[4] - y[5] - 30.0;
	g8 = -3.0 * y[0] + 6.0 * y[1] + 12.0 * (y[8] - 8.0) * (y[8] - 8.0) - 7.0 * y[9];  

    count_eval = true; // count a black-box evaluation
	  
    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); // constraints value
    x.set_bb_output  ( 2 , g2  );
    x.set_bb_output  ( 3 , g3  ); 
    x.set_bb_output  ( 4 , g4  ); 
    x.set_bb_output  ( 5 , g5  ); 
    x.set_bb_output  ( 6 , g6  ); 
    x.set_bb_output  ( 7 , g7  ); 
    x.set_bb_output  ( 8 , g8  ); 

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

  int dim = 10;
  int cont = 8;
  int nbpoints = 30;
  int nbiter = 100;

  p.set_DIMENSION (dim);             // number of variables

  vector<bb_output_type> bbot (cont+1); // definition of
  bbot[0] = OBJ;                   // output types
  bbot[1] = PB;
  bbot[2] = PB;
  bbot[3] = PB;
  bbot[4] = PB;
  bbot[5] = PB;
  bbot[6] = PB;
  bbot[7] = PB;
  bbot[8] = PB;
  p.set_BB_OUTPUT_TYPE ( bbot );
  
  Point x0(dim); // starting points
  string temp;
  int temp2;
  ifstream fich("DOE.plan");
  for (int k=0; k<7; k++)
    {
      getline(fich,temp);
    }
  for (int j=0; j<nbpoints; j++)
    {
      fich >> temp2 >> temp;
      for (int k=0; k<6; k++)
	{
      fich >> x0[k];
      if (x0[k]==-10.0)
	x0[k]=0;
      else if (x0[k]==-5.0)
	x0[k]=1;
      else if (x0[k]==0.0)
	x0[k]=2;
      else if (x0[k]==1.3)
	x0[k]=3;
      else if (x0[k]==2.2)
	x0[k]=4;
      else if (x0[k]==5.0)
	x0[k]=5;
      else if (x0[k]==8.2)
	x0[k]=6;
      else if (x0[k]==8.7)
	x0[k]=7;
      else if (x0[k]==9.5)
	x0[k]=8;
      else
	x0[k]=9;
      }
      fich >> x0[6] >> x0[7] >> x0[8] >> x0[9];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  x0[0]=0;
  x0[1]=0;
  x0[2]=0;
  x0[3]=0;
  x0[4]=0;
  x0[5]=0;
  x0[6]=-10;
  x0[7]=-10;
  x0[8]=-10;
  x0[9]=-10;
  p.set_LOWER_BOUND ( x0 );
  x0[0]=9;
  x0[1]=9;
  x0[2]=9;
  x0[3]=9;
  x0[4]=9;
  x0[5]=9;
  x0[6]=10;
  x0[7]=10;
  x0[8]=10;
  x0[9]=10;
  p.set_UPPER_BOUND ( x0 );

  p.set_BB_INPUT_TYPE (0 , INTEGER);
  p.set_BB_INPUT_TYPE (1 , INTEGER);
  p.set_BB_INPUT_TYPE (2 , INTEGER);
  p.set_BB_INPUT_TYPE (3 , INTEGER);
  p.set_BB_INPUT_TYPE (4 , INTEGER);
  p.set_BB_INPUT_TYPE (5 , INTEGER);
  p.set_BB_INPUT_TYPE (8 , INTEGER);
  p.set_BB_INPUT_TYPE (9 , INTEGER);

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
