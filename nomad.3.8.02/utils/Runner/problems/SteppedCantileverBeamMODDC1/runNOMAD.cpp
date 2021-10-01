#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=11                                */
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
    Double f,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11;
    vector<Double> y(10);
    for (int k=0; k<10; k++){
      y[k]=x[k];
    }

    int l=100;
    int P=50000;
    double deltamax=2.7;
    double sigmamax=14000;
    int E=20000000;

    switch (static_cast<int>(x[1].value())) {
    case 0:
      y[1]=45;
      break;
    case 1:
      y[1]=50;
      break;
    case 2:
      y[1]=55;
      break;
    case 3:
      y[1]=60;
      break;
    }
	  
    switch (static_cast<int>(x[2].value())) {
    case 0:
      y[2]=2.4;
      break;
    case 1:
      y[2]=2.6;
      break;
    case 2:
      y[2]=2.8;
      break;
    case 3:
      y[2]=3.1;
      break;
    }
	
    switch (static_cast<int>(x[3].value())) {
    case 0:
      y[3]=45;
      break;
    case 1:
      y[3]=50;
      break;
    case 2:
      y[3]=55;
      break;
    case 3:
      y[3]=60;
      break;
    }
	  
    switch (static_cast<int>(x[4].value())) {
    case 0:
      y[4]=2.4;
      break;
    case 1:
      y[4]=2.6;
      break;
    case 2:
      y[4]=2.8;
      break;
    case 3:
      y[4]=3.1;
      break;
    }
    
    // Objective function
    f = l*(y[0]*y[1]+y[2]*y[3]+y[4]*y[5]+y[6]*y[7]+y[8]*y[9]) ; 
    
    // Constraints
    g1 = (6*P*l)-sigmamax*y[8]*y[9]*y[9];
    g2 = (12*P*l)-sigmamax*y[6]*y[7]*y[7];
    g3 = (18*P*l)-sigmamax*y[4]*y[5]*y[5];
    g4 = (24*P*l)-sigmamax*y[2]*y[3]*y[3];
    g5 = (30*P*l)-sigmamax*y[0]*y[1]*y[1];
    g6 = ((P*l*l*l)/E)*(244*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+148*y[0]*y[1]*y[1]*y[1]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+76*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+28*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[8]*y[9]*y[9]*y[9]+4*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7])-deltamax*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9];
    g7 = y[1]-20*y[0]; 
    g8 = y[3]-20*y[2];  
    g9 = y[5]-20*y[4];  
    g10 = y[7]-20*y[6];  
    g11 = y[9]-20*y[8];   
    
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
  vector<int> other_types2;
  vector<int> other_types3;
  vector<int> other_types4;

  for (int j=0; j<4; j++)
	{
	  if (x[1].value() != j)
		{
		  other_types1.push_back(j);
		}
	}
  for ( size_t k = 0 ; k < other_types1.size() ; k++ )
    {
      Point y = x ;
      y[1] = other_types1[k];		
      add_extended_poll_point ( y , *(x.get_signature())  );
    }
  for (int j=0; j<4; j++)
	{
	  if (x[2].value() != j)
		{
		  other_types2.push_back(j);
		}
	}
  for ( size_t k = 0 ; k < other_types2.size() ; k++ )
    {
      Point y = x ;
      y[2] = other_types2[k];		
      add_extended_poll_point ( y , *(x.get_signature())  );
    }
  for (int j=0; j<4; j++)
	{
	  if (x[3].value() != j)
		{
		  other_types3.push_back(j);
		}
	}
  for ( size_t k = 0 ; k < other_types3.size() ; k++ )
    {
      Point y = x ;
      y[3] = other_types3[k];		
      add_extended_poll_point ( y , *(x.get_signature())  );
    }
  for (int j=0; j<4; j++)
	{
	  if (x[4].value() != j)
		{
		  other_types4.push_back(j);
		}
	}
  for ( size_t k = 0 ; k < other_types4.size() ; k++ )
    {
      Point y = x ;
      y[4] = other_types4[k];		
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

  int dim = 10;
  int cont = 11;
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
  for (int j=0; j<nbpoints; j++)
    {
      fich >> temp2 >> temp;
      fich >> x0[0];
      fich >> x0[1];
      if (x0[1]==45)
	x0[1]=0;
      else if (x0[1]==50)
	x0[1]=1;
      else if (x0[1]==55)
	x0[1]=2;
      else if (x0[1]==60)
	x0[1]=3;
      fich >> x0[2];
      if (x0[2]==2.4)
	x0[2]=0;
      else if (x0[2]==2.6)
	x0[2]=1;
      else if (x0[2]==2.8)
	x0[2]=2;
      else if (x0[2]==3.1)
	x0[2]=3;
      fich >> x0[3];
      if (x0[3]==45)
	x0[3]=0;
      else if (x0[3]==50)
	x0[3]=1;
      else if (x0[3]==55)
	x0[3]=2;
      else if (x0[3]==60)
	x0[3]=3;
      fich >> x0[4];
      if (x0[4]==2.4)
	x0[4]=0;
      else if (x0[4]==2.6)
	x0[4]=1;
      else if (x0[4]==2.8)
	x0[4]=2;
      else if (x0[4]==3.1)
	x0[4]=3;
      fich >> x0[5] >> x0[6] >> x0[7] >> x0[8] >> x0[9];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  Point b(dim); 
  b[0]=1;
  b[5]=30;
  b[6]=1;
  b[7]=30;
  b[8]=1;
  b[9]=30;
  p.set_LOWER_BOUND ( b );
  b[0]=5;
  b[5]=65;
  b[6]=5;
  b[7]=65;
  b[8]=5;
  b[9]=65;
  p.set_UPPER_BOUND ( b );

  p.set_BB_INPUT_TYPE (0 , INTEGER);
  p.set_BB_INPUT_TYPE (1 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (2 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (3 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (4 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (5 , INTEGER);

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
