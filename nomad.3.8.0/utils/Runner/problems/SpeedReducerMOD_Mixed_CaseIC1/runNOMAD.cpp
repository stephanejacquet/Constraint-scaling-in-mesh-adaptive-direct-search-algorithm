#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=4, m=3                                 */
/*       PressureVessel Mixed Case                       */
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
    double x1, x2, x3, x4, x5, x6, x7;
    double f;
    double A, B, C, D, A1, A2, B1, B2;
    double g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11;

    x1=x[0].value();
    x2=x[1].value();
    x3=x[2].value();
    x4=x[3].value();
    x5=x[4].value();
    x6=x[5].value();
    x7=x[6].value();

    // Cost function
    A = 3.3333 * x3 * x3 + 14.9334 * x3 - 43.0934;
    B = x6 * x6 + x7 * x7;
    C = x6 * x6 * x6 + x7 * x7 * x7;
    D = x4 * x6 * x6 + x5 * x7 * x7;
    f = 0.7854 * x1 * x2 * x2 * A - 1.508 * x1 * B + 7.477 * C + 0.7854 * D;

    // Constraints
    A1 = std::sqrt(745.0 * x4 * 745.0 * x4 + 16900000.0 * x2 * x2 * x3 * x3);
    A2 = std::sqrt(745.0 * x5 * 745.0 * x5 + 157500000.0 * x2 * x2 * x3 * x3);
    g1 = 27.0 - x1 * x2 * x2 * x3;                                // <= 0
    g2 = 397.5 - x1 * x2 * x2 * x3 * x3;                          // <= 0
    g3 = 1.93 * x4 * x4 * x4 - x2 * x6 * x6 * x6 * x6 * x3;       // <= 0
    g4 = 1.93 * x5 * x5 * x5 - x2 * x7 * x7 * x7 * x7 * x3;       // <= 0
    g5 = A1 - 110.0 * x2 * x3 * x6 * x6 * x6;                     // <= 0
    g6 = A2 - 85.0 * x2 * x3 * x7 * x7 * x7;                      // <= 0
    g7 = x2 * x3 - 40.0;                                          // <= 0
    g8 = 5.0 * x2 - x1;                                           // <= 0
    g9 = x1 - 12 * x2;                                            // <= 0
    g10 = 1.9 + 1.5 * x6 - x4;                                    // <= 0
    g11 = 1.9 + 1.1 * x7 - x5;                                    // <= 0

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); 
    x.set_bb_output  ( 2 , g2  ); 
    x.set_bb_output  ( 3 , g3  ); 
    x.set_bb_output  ( 4 , g4  ); 
    x.set_bb_output  ( 5 , g5  ); 
    x.set_bb_output  ( 6 , g6  ); 
    x.set_bb_output  ( 7 , g7  ); 
    x.set_bb_output  ( 8 , g8  ); 
    x.set_bb_output  ( 9 , g9  ); 
    x.set_bb_output  ( 10 , g10  ); 
    x.set_bb_output  ( 11 , g11  ); 

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

  for (int j=17; j<29; j++)
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

  int dim = 7;
  int cont = 11;
  int nbpoints = 21;
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
  bbot[9] = PB;                   
  bbot[10] = PB;                   
  bbot[11] = PB;                                     
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

  Point b(dim);
  b[0]=2.6;
  b[1]=0.7;
  b[3]=7.3;
  b[4]=7.3;
  b[5]=2.9;
  b[6]=5.0;
  p.set_LOWER_BOUND ( b );
  b[0]=3.6;
  b[1]=0.8;
  b[3]=8.3;
  b[4]=8.3;
  b[5]=3.9;
  b[6]=5.5;
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
