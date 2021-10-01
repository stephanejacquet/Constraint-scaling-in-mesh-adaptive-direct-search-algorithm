/*-----------------------------------------------------*/
/*  how to use the NOMAD library with a user function  */
/*-----------------------------------------------------*/
#include "nomad.hpp"
using namespace std;
// using namespace NOMAD; avoids putting NOMAD:: everywhere


const int N=10;

/*----------------------------------------*/
/*               The problem              */
/*----------------------------------------*/
class My_Evaluator : public NOMAD::Evaluator {
public:
  My_Evaluator  ( const NOMAD::Parameters & p ) :
    NOMAD::Evaluator ( p ) {}

  ~My_Evaluator ( void ) {}

  bool eval_x ( NOMAD::Eval_Point   & x          ,
		const NOMAD::Double & h_max      ,
		bool                & count_eval   ) const
	{

        int    i ;
        double z = 0.0;
        
        double sum1 = 0.0 , sum2 = 0.0 , sum3 = 0.0 , prod1 = 1.0 , prod2 = 1.0;
        
        for ( i = 0 ; i < N ; ++i ) {
            sum1  += pow ( cos(x[i].value()) , 4 );
            sum2  += x[i].value();
            sum3  += (i+1)*x[i].value()*x[i].value();
            prod1 *= pow ( cos(x[i].value()) , 2 );
            prod2 *= x[i].value();
        }
        
        double g1 = -prod2+0.75;
        double g2 = sum2 -7.5*N;
        
        if ( sum3 == 0.0 )
            z = 1e+20;
        else
            z  = - fabs ( ( sum1 - 2 * prod1 ) / sqrt(sum3) );
        
        
        x.set_bb_output ( 0 , g1 );
        x.set_bb_output ( 1 , g2 );
        x.set_bb_output ( 2 , z  );
        
        count_eval = true;
        
        return true;
    }
	
	
};

/*------------------------------------------*/
/*            NOMAD main function           */
/*------------------------------------------*/
int main ( int argc , char ** argv ) {

  // display:
  NOMAD::Display out ( std::cout );
  out.precision ( NOMAD::DISPLAY_PRECISION_STD );

  try {

    // NOMAD initializations:
    NOMAD::begin ( argc , argv );

    // parameters creation:
    NOMAD::Parameters p ( out );

    //
    p.set_DIMENSION (N);             // number of variables

    vector<NOMAD::bb_output_type> bbot (3); // definition of
    bbot[0] = NOMAD::PB;                   // output types
    bbot[1] = NOMAD::PB;
    bbot[2] = NOMAD::OBJ;
    p.set_BB_OUTPUT_TYPE ( bbot );

    p.set_DISPLAY_STATS ( "bbe ( sol ) obj" );

    p.set_X0 ( NOMAD::Point(N,5.0) );  // starting point

    p.set_LOWER_BOUND ( NOMAD::Point ( N , 0.0 ) ); // all var. >= 0
    p.set_UPPER_BOUND ( NOMAD::Point ( N , 10.0 ) ); // all var. >= 0
      
    p.set_MAX_BB_EVAL (N*1000);
      
    p.set_DISPLAY_DEGREE(2);
    p.set_SOLUTION_FILE("sol.txt");

    // parameters validation:
    p.check();

    // custom evaluator creation:
    My_Evaluator ev   ( p );

    // algorithm creation and execution:
    NOMAD::Mads mads ( p , &ev );
    mads.run();
  }
  catch ( exception & e ) {
    cerr << "\nNOMAD has been interrupted (" << e.what() << ")\n\n";
  }

  NOMAD::Slave::stop_slaves ( out );
  NOMAD::end();

  return EXIT_SUCCESS;
}
