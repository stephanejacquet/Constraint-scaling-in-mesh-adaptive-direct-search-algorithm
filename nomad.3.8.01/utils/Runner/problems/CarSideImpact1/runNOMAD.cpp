#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=11, m=10                               */
/*       SteppedCantileverBeam                          */
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
    Double f,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10;
    vector<Double> y(11);
    for (int k=0; k<11; k++){
      y[k]=x[k];
    }

    for(int k=7; k<9; k++){
    switch (static_cast<int>(x[k].value())) {
    case 0:
      y[k]=0.192;
      break;
    case 1:
      y[k]=0.345;
      break;
    }
    }
    
    // Objective function
    f =  1.98+4.9*y[0]+6.67*y[1]+6.98*y[2]+4.01*y[3]+1.78*y[4]+2.73*y[6] ; 
    
    // Constraints
    g1 = 1.16-0.3717*y[1]*y[3]-0.00931*y[1]*y[9]-0.484*y[2]*y[8]+0.01343*y[5]*y[9]-1;
  g2 = 0.261-0.0159*y[0]*y[1]-0.188*y[0]*y[7]-0.019*y[1]*y[6]+0.0144*y[2]*y[4]+0.0008757*y[4]*y[9]+0.08045*y[5]*y[8]+0.00139*y[7]*y[10]+0.00001575*y[9]*y[10]-0.32;
  g3 = 0.214+0.00817*y[4]-0.131*y[0]*y[7]-0.0704*y[0]*y[8]+0.03099*y[1]*y[5]-0.018*y[1]*y[6]+0.0208*y[2]*y[7]+0.121*y[2]*y[8]-0.00364*y[4]*y[5]+0.0007715*y[4]*y[9]-0.0005354*y[5]*y[9]+0.00121*y[7]*y[10]+0.00184*y[8]*y[9]-0.02*y[1]*y[1]-0.32;  
  g4 = 0.74-0.61*y[1]-0.163*y[2]*y[7]+0.001232*y[2]*y[9]-0.166*y[6]*y[8]+0.227*y[1]*y[1]-0.32;
  g5 = 28.98+3.818*y[2]-4.2*y[0]*y[1]+0.0207*y[4]*y[9]+6.63*y[5]*y[8]-7.7*y[6]*y[7]+0.32*y[8]*y[9]-32;
  g6 = 33.86+2.95*y[2]+0.1792*y[9]-5.057*y[0]*y[1]-11*y[1]*y[7]-0.0215*y[4]*y[9]-9.98*y[6]*y[7]+22*y[7]*y[8]-32;
  g7 = 46.36-9.9*y[1]-12.9*y[0]*y[7]+0.1107*y[2]*y[9]-32; 
  g8 = 4.72-0.5*y[3]-0.19*y[1]*y[2]-0.0122*y[3]*y[9]+0.009325*y[5]*y[9]+0.000191*y[10]*y[10]-4;  
  g9 = 10.58-0.674*y[0]*y[1]-1.95*y[1]*y[7]+0.02054*y[2]*y[9]-0.0198*y[3]*y[9]+0.028*y[5]*y[9]-9.9;  
  g10 = 16.45-0.489*y[2]*y[6]-0.843*y[4]*y[5]+0.0432*y[8]*y[9]-0.0556*y[8]*y[10]-0.000786*y[10]*y[10]-15.7;  
    
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
    x.set_bb_output  ( 9 , g9  ); 
    x.set_bb_output  ( 10 , g10  ); 

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

  int dim = 11;
  int cont = 10;
  int nbpoints = 33;
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
      fich >> x0[0];
      fich >> x0[1];
      fich >> x0[2];
      fich >> x0[3];
      fich >> x0[4];
      fich >> x0[5];
      fich >> x0[6];
      fich >> x0[7];
      if(x0[7]==0.192)
	x0[7]=0;
      else
	x0[7]=1;
      fich >> x0[8];
      if(x0[8]==0.192)
	x0[8]=0;
      else
	x0[8]=1;
      fich >> x0[9];
      fich >> x0[10];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  x0[0]=0.5;
  x0[1]=0.5;
  x0[2]=0.5;
  x0[3]=0.5;
  x0[4]=0.5;
  x0[5]=0.5;
  x0[6]=0.5;
  x0[7]=0;
  x0[8]=0;
  x0[9]=-30;
  x0[10]=-30;
  p.set_LOWER_BOUND ( x0 );
  x0[0]=1.5;
  x0[1]=1.5;
  x0[2]=1.5;
  x0[3]=1.5;
  x0[4]=1.5;
  x0[5]=1.5;
  x0[6]=1.5;
  x0[7]=1;
  x0[8]=1;
  x0[9]=30;
  x0[10]=30;
  p.set_UPPER_BOUND ( x0 );

  p.set_BB_INPUT_TYPE (7 , INTEGER);
  p.set_BB_INPUT_TYPE (8 , INTEGER);

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
