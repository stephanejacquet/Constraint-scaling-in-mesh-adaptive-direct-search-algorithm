#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=3, m=4                                 */
/*       Spring Mixed Case                       */
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

   Double f, g1, g2, g3, g4;
  
  // Cost function
  f = (x[2]+2)*x[0]*x[1]*x[1];
  
  // Constraints
  g1 =  (71785*x[1]*x[1]*x[1]*x[1]) - (x[0]*x[0]*x[0]*x[2]);   // <= 0
  g2 = 5108 * (4*x[0]*x[0]-x[0]*x[1])*x[1]*x[1] + (12566*x[0]-x[1])*x[1]*x[1]*x[1] - 5108*(12566*x[0]-x[1])*x[1]*x[1]*x[1]*x[1]*x[1];  // <= 0
  g3 = (x[0]*x[0]*x[2])-(140.45*x[1]);  // <= 0
  g4 = x[0]+x[1]-1.5; // <= 0

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); 
    x.set_bb_output  ( 2 , g2  ); 
    x.set_bb_output  ( 3 , g3  ); 
    x.set_bb_output  ( 4 , g4  ); 

    return true;       // the evaluation succeeded
  }
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class My_Extended_Poll : public Extended_Poll
{
public:
	
  // constructor:
  My_Extended_Poll ( Parameters & p): 
    Extended_Poll ( p    ) {}
	
  // destructor:
  virtual ~My_Extended_Poll ( void ) {}
	
  // construct the extended poll points:
  virtual void construct_extended_points ( const Eval_Point & );
	
};


/*--------------------------------------*/
/*  construct the extended poll points  */
/*      (categorical neighborhoods)     */
/*--------------------------------------*/
void My_Extended_Poll::construct_extended_points ( const Eval_Point & x )
{
  int cur;
  vector<int> other_types1;

  for (int j=2; j<16; j++)
	{
	  if (x[2].value() != j)
		{
		  other_types1.push_back(j);
		}
	}
  for ( size_t k = 0 ; k < other_types1.size() ; k++ )
    {
      Point y = x ;
      y[2] = other_types1[k];		
      add_extended_poll_point ( y , *(x.get_signature())  );
    }
}

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

  int dim = 3;
  int cont = 4;
  int nbpoints = 9;
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
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  Point b(dim);
  b[0]=0.25;
  b[1]=0.05;
  p.set_LOWER_BOUND ( b );
  b[0]=1.3;
  b[1]=2.0;
  p.set_UPPER_BOUND ( b );

  p.set_BB_INPUT_TYPE (2 , CATEGORICAL);

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

  // extended poll:
  My_Extended_Poll ep ( p );

  // algorithm creation:
  Mads mads ( p , &ev , &ep, NULL, NULL);

  // algorithm execution:
  mads.run();

  Slave::stop_slaves ( out );
  end();

  return EXIT_SUCCESS;
}
