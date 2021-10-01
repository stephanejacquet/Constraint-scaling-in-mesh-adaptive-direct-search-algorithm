#include <string> 
#include "nomad.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=3, m=2                                 */
/*       Reinforced Concrete Beam Mixed Case      */
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

   Double f, g1, g2;
    vector<Double> y(3);
    for (int k=0; k<3; k++){
      y[k]=x[k];
    }

    switch (static_cast<int>(x[0].value())){
    case 0:
      y[0]=0.2;
      break;
    case 1:
      y[0]=0.31;
      break;
    case 2:
      y[0]=0.4;
      break;
    case 3:
      y[0]=0.44;
      break;
    case 4:
      y[0]=0.6;
      break;
    case 5:
      y[0]=0.62;
      break;
    case 6:
      y[0]=0.79;
      break;
    case 7:
      y[0]=0.8;
      break;
    case 8:
      y[0]=0.88;
      break;
    case 9:
      y[0]=0.93;
      break;
    case 10:
      y[0]=1;
      break;
    case 11:
      y[0]=1.2;
      break;
    case 12:
      y[0]=1.24;
      break;
    case 13:
      y[0]=1.32;
      break;
    case 14:
      y[0]=1.4;
      break;
    case 15:
      y[0]=1.55;
      break;
    case 16:
      y[0]=1.58;
      break;
    case 17:
      y[0]=1.6;
      break;
    case 18:
      y[0]=1.76;
      break;
    case 19:
      y[0]=1.8;
      break;
    case 20:
      y[0]=1.86;
      break;
    case 21:
      y[0]=2;
      break;
    case 22:
      y[0]=2.17;
      break;
    case 23:
      y[0]=2.2;
      break;
    case 24:
      y[0]=2.37;
      break;
    case 25:
      y[0]=2.4;
      break;
    case 26:
      y[0]=2.48;
      break;
    case 27:
      y[0]=2.6;
      break;
    case 28:
      y[0]=2.64;
      break;
    case 29:
      y[0]=2.79;
      break;
    case 30:
      y[0]=2.8;
      break;
    case 31:
      y[0]=3;
      break;
    case 32:
      y[0]=3.08;
      break;
    case 33:
      y[0]=3.1;
      break;
    case 34:
      y[0]=3.16;
      break;
    case 35:
      y[0]=3.41;
      break;
    case 36:
      y[0]=3.52;
      break;
    case 37:
      y[0]=3.6;
      break;
    case 38:
      y[0]=3.72;
      break;
    case 39:
      y[0]=3.95;
      break;
    case 40:
      y[0]=3.96;
      break;
    case 41:
      y[0]=4;
      break;
    case 42:
      y[0]=4.03;
      break;
    case 43:
      y[0]=4.2;
      break;
    case 44:
      y[0]=4.34;
      break;
    case 45:
      y[0]=4.4;
      break;
    case 46:
      y[0]=4.65;
      break;
    case 47:
      y[0]=4.74;
      break;
    case 48:
      y[0]=4.8;
      break;
    case 49:
      y[0]=4.84;
      break;
    case 50:
      y[0]=5;
      break;
    case 51:
      y[0]=5.28;
      break;
    case 52:
      y[0]=5.4;
      break;
    case 53:
      y[0]=5.53;
      break;
    case 54:
      y[0]=5.72;
      break;
    case 55:
      y[0]=6;
      break;
    case 56:
      y[0]=6.16;
      break;
    case 57:
      y[0]=6.32;
      break;
    case 58:
      y[0]=6.6;
      break;
    case 59:
      y[0]=7.11;
      break;
    case 60:
      y[0]=7.2;
      break;
    case 61:
      y[0]=7.8;
      break;
    case 62:
      y[0]=7.9;
      break;
    case 63:
      y[0]=8;
      break;
    case 64:
      y[0]=8.4;
      break;
    case 65:
      y[0]=8.69;
      break;
    case 66:
      y[0]=9;
      break;
    case 67:
      y[0]=9.48;
      break;
    case 68:
      y[0]=10.27;
      break;
    case 69:
      y[0]=11;
      break;
    case 70:
      y[0]=11.06;
      break;
    case 71:
      y[0]=11.85;
      break;
    case 72:
      y[0]=12;
      break;
    case 73:
      y[0]=13;
      break;
    case 74:
      y[0]=14;
      break;
    case 75:
      y[0]=15;
      break;
    }
  
  // Cost function
 f = 29.4*y[0]+0.6*y[1]*y[2];
  
  // Constraints
  g1 = y[1]-4*y[2];   // <= 0
  g2 = 180*y[2]+7.375*y[0]*y[0]-y[0]*y[1]*y[2];  // <= 0

    count_eval = true; // count a black-box evaluation

    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); 
    x.set_bb_output  ( 2 , g2  ); 

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

  int dim = 3;
  int cont = 2;
  int nbpoints = 9;
  int nbiter = 100;

  p.set_DIMENSION (dim);             // number of variables

  vector<bb_output_type> bbot (cont+1); // definition of
  bbot[0] = OBJ;                   // output types
  bbot[1] = PB;                   
  bbot[2] = PB;                   
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
      if (x0[0]==0.2)
	x0[0]=0;
      else if (x0[0]==0.31)
	x0[0]=1;
      else if (x0[0]==0.4)
	x0[0]=2;
      else if (x0[0]==0.44)
	x0[0]=3;
      else if (x0[0]==0.6)
	x0[0]=4;
      else if (x0[0]==0.62)
	x0[0]=5;
      else if (x0[0]==0.79)
	x0[0]=6;
      else if (x0[0]==0.8)
	x0[0]=7;
      else if (x0[0]==0.88)
	x0[0]=8;
      else if (x0[0]==0.93)
	x0[0]=9;
      else if (x0[0]==1)
	x0[0]=10;
      else if (x0[0]==1.2)
	x0[0]=11;
      else if (x0[0]==1.24)
	x0[0]=12;
      else if (x0[0]==1.32)
	x0[0]=13;
      else if (x0[0]==1.4)
	x0[0]=14;
      else if (x0[0]==1.55)
	x0[0]=15;
      else if (x0[0]==1.58)
	x0[0]=16;
      else if (x0[0]==1.6)
	x0[0]=17;
      else if (x0[0]==1.76)
	x0[0]=18;
      else if (x0[0]==1.8)
	x0[0]=19;
      else if (x0[0]==1.86)
	x0[0]=20;
      else if (x0[0]==2)
	x0[0]=21;
      else if (x0[0]==2.17)
	x0[0]=22;
      else if (x0[0]==2.2)
	x0[0]=23;
      else if (x0[0]==2.37)
	x0[0]=24;
      else if (x0[0]==2.4)
	x0[0]=25;
      else if (x0[0]==2.48)
	x0[0]=26;
      else if (x0[0]==2.6)
	x0[0]=27;
      else if (x0[0]==2.64)
	x0[0]=28;
      else if (x0[0]==2.79)
	x0[0]=29;
      else if (x0[0]==2.8)
	x0[0]=30;
      else if (x0[0]==3)
	x0[0]=31;
      else if (x0[0]==3.08)
	x0[0]=32;
      else if (x0[0]==3.1)
	x0[0]=33;
      else if (x0[0]==3.16)
	x0[0]=34;
      else if (x0[0]==3.41)
	x0[0]=35;
      else if (x0[0]==3.52)
	x0[0]=36;
      else if (x0[0]==3.6)
	x0[0]=37;
      else if (x0[0]==3.72)
	x0[0]=38;
      else if (x0[0]==3.95)
	x0[0]=39;
      else if (x0[0]==3.96)
	x0[0]=40;
      else if (x0[0]==4)
	x0[0]=41;
      else if (x0[0]==4.03)
	x0[0]=42;
      else if (x0[0]==4.2)
	x0[0]=43;
      else if (x0[0]==4.34)
	x0[0]=44;
      else if (x0[0]==4.4)
	x0[0]=45;
      else if (x0[0]==4.65)
	x0[0]=46;
      else if (x0[0]==4.74)
	x0[0]=47;
      else if (x0[0]==4.8)
	x0[0]=48;
      else if (x0[0]==4.84)
	x0[0]=49;
      else if (x0[0]==5)
	x0[0]=50;
      else if (x0[0]==5.28)
	x0[0]=51;
      else if (x0[0]==5.4)
	x0[0]=52;
      else if (x0[0]==5.53)
	x0[0]=53;
      else if (x0[0]==5.72)
	x0[0]=54;
      else if (x0[0]==6)
	x0[0]=55;
      else if (x0[0]==6.16)
	x0[0]=56;
      else if (x0[0]==6.32)
	x0[0]=57;
      else if (x0[0]==6.6)
	x0[0]=58;
      else if (x0[0]==7.11)
	x0[0]=59;
      else if (x0[0]==7.2)
	x0[0]=60;
      else if (x0[0]==7.8)
	x0[0]=61;
      else if (x0[0]==7.9)
	x0[0]=62;
      else if (x0[0]==8)
	x0[0]=63;
      else if (x0[0]==8.4)
	x0[0]=64;
      else if (x0[0]==8.69)
	x0[0]=65;
      else if (x0[0]==9)
	x0[0]=66;
      else if (x0[0]==9.48)
	x0[0]=67;
      else if (x0[0]==10.27)
	x0[0]=68;
      else if (x0[0]==11)
	x0[0]=69;
      else if (x0[0]==11.06)
	x0[0]=70;
      else if (x0[0]==11.85)
	x0[0]=71;
      else if (x0[0]==12)
	x0[0]=72;
      else if (x0[0]==13)
	x0[0]=73;
      else if (x0[0]==14)
	x0[0]=74;
      else if (x0[0]==15)
	x0[0]=75;
      fich >> x0[1];
      fich >> x0[2];
      getline(fich,temp);
      p.set_X0 ( x0 );
    }
  fich.close();

  Point x1(dim);
  x1[0]=0;
  x1[1]=28;
  x1[2]=5.0;
  p.set_LOWER_BOUND ( x1 );
  x1[0]=75;
  x1[1]=40;
  x1[2]=10.0;
  p.set_UPPER_BOUND ( x1 );

  p.set_BB_INPUT_TYPE (0 , INTEGER);
  p.set_BB_INPUT_TYPE (1 , INTEGER);

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
