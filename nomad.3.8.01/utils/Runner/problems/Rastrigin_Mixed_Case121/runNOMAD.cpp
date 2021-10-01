#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=0                                 */
/*       Rastrigin Mixed Case12                       */
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
    
    double f;
    double pi = 3.1415926535897932385;
    int n=10;
    vector<double> y(10);
    for (int k=0; k<10; k++){
      y[k]=x[k].value();
    }
    
    for (int k=0; k<2; k++) {
      switch (static_cast<int>(x[k].value())) {
      case 0:
	y[k]=-5;
	break;
      case 1:
	y[k]=-3;
	break;
      case 2:
	y[k]=-1;
	break;
      case 3:
	y[k]=0;
	break;
      case 4:
	y[k]=1;
	break;
      case 5:
	y[k]=3;
	break;
      case 6:
	y[k]=5;
	break;
      }
    }
    switch (static_cast<int>(x[2].value())) {
    case 0:
      y[2]=-5;
      break;
    case 1:
      y[2]=0;
      break;
    case 2:
      y[2]=2;
      break;
    case 3:
      y[2]=5;
      break;
    }

    f = 10.0 * n;
    for(unsigned long int i = 0; i < n; ++i)
      f += y[i] * y[i] - 10.0 * cos(2.0 * pi * y[i]);

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value

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
  for (int k=3; k<5; k++){
    int cur = static_cast<int> (x[k].value());
    vector<int> other_types;
    switch ( cur )
      {
	case -5:
      other_types.push_back(-3);
      other_types.push_back(-1);
      other_types.push_back(0);
      other_types.push_back(1);
      other_types.push_back(3);
      other_types.push_back(5);
      break;
    case -3:
      other_types.push_back(-5);
      other_types.push_back(-1);
      other_types.push_back(0);
      other_types.push_back(1);
      other_types.push_back(3);
      other_types.push_back(5);
      break;
    case -1:
      other_types.push_back(-5);
      other_types.push_back(-3);
      other_types.push_back(0);
      other_types.push_back(1);
      other_types.push_back(3);
      other_types.push_back(5);
      break;
    case 0:
      other_types.push_back(-5);
      other_types.push_back(-3);
      other_types.push_back(-1);
      other_types.push_back(1);
      other_types.push_back(3);
      other_types.push_back(5);
      break;
    case 1:
      other_types.push_back(-5);
      other_types.push_back(-3);
      other_types.push_back(-1);
      other_types.push_back(0);
      other_types.push_back(3);
      other_types.push_back(5);
      break;
    case 3:
      other_types.push_back(-5);
      other_types.push_back(-3);
      other_types.push_back(-1);
      other_types.push_back(0);
      other_types.push_back(1);
      other_types.push_back(5);
      break;
    case 5:
      other_types.push_back(-5);
      other_types.push_back(-3);
      other_types.push_back(-1);
      other_types.push_back(0);
      other_types.push_back(1);
      other_types.push_back(3);
      break;
      }
    for ( size_t j = 0 ; j < other_types.size() ; j++ )
      {
	Point y = x ;
	y[k] = other_types[j];	
	add_extended_poll_point ( y , *(x.get_signature())  );
      }
  }
    int cur = static_cast<int> (x[5].value());
    vector<int> other_types;
    switch ( cur )
      {
      case 0:
	other_types.push_back(1);
	other_types.push_back(2);
	other_types.push_back(3);
	break;
      case 1:
	other_types.push_back(0);
	other_types.push_back(2);
	other_types.push_back(3);
	break;
      case 2:
	other_types.push_back(0);
	other_types.push_back(1);
	other_types.push_back(3);
      case 3:
	other_types.push_back(0);
	other_types.push_back(1);
	other_types.push_back(2);
	break;
      }
    for ( size_t j = 0 ; j < other_types.size() ; j++ )
      {
	Point y = x ;
	y[5] = other_types[j];	
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
  int cont = 0;
  int nbpoints = 30;
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
      for (int j=0; j<2; j++){
	fich >> x0[j]; 
	if (x0[j].value()==-5)
	  x0[j]=0;
	else if (x0[j].value()==-3)
	  x0[j]=1;
	else if (x0[j].value()==-1)
	  x0[j]=2;
	else if (x0[j].value()==0)
	  x0[j]=3;
	else if (x0[j].value()==1)
	  x0[j]=4;
	else if (x0[j].value()==3)
	  x0[j]=5;
	else if (x0[j].value()==5)
	  x0[j]=6;
      }
      fich >> x0[2];
      if (x0[2].value()==-5)
	x0[2]=0;
      else if (x0[2].value()==0)
	x0[2]=1;
      else if (x0[2].value()==2)
	x0[2]=2;
      else if (x0[2].value()==5)
	x0[2]=3;
      for (int j=3; j<10; j++){
	fich >> x0[j];
      }
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();
  
  Point x1(dim);
  x1[0]=0;
  x1[1]=0;
  x1[2]=0;
  x1[6]=-5.0;
  x1[7]=-5.0;
  x1[8]=-5;
  x1[9]=-5;
  p.set_LOWER_BOUND ( x1 );
  x1[0]=6;
  x1[1]=6;
  x1[2]=3;
  x1[6]=5.0;
  x1[7]=5.0;
  x1[8]=5;
  x1[9]=5;
  p.set_UPPER_BOUND ( x1 );

  p.set_BB_INPUT_TYPE (0 , INTEGER);
  p.set_BB_INPUT_TYPE (1 , INTEGER);
  p.set_BB_INPUT_TYPE (2 , INTEGER);
  p.set_BB_INPUT_TYPE (3 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (4 , CATEGORICAL);
  p.set_BB_INPUT_TYPE (5 , CATEGORICAL);
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
