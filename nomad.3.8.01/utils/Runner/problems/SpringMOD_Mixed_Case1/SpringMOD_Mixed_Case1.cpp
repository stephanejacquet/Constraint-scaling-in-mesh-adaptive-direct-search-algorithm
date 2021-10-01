#include <string>
#include "SpringMOD_Mixed_Case1.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=3, m=5                                 */
/*       SpringMOD Mixed Case 1                   */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
SpringMOD_Mixed_Case1::SpringMOD_Mixed_Case1 ( void )
: Problem ( "SpringMOD_Mixed_Case1" , "SpringMOD_Mixed_Case1" , "xe.txt" , 3 , 5 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );
    set_bbot ( 1 , NOMAD::PB );
    set_bbot ( 2 , NOMAD::PB );
    set_bbot ( 3 , NOMAD::PB );
    set_bbot ( 4 , NOMAD::PB );

    int dim = 3;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/SpringMOD_Mixed_Case1/DOE.plan");
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
        set_x0 ( x0 );
    }

    fich.close();
    
    Point xl(dim);
    xl[0]=0.25;
    xl[1]=0.05;
    xl[2]=2;


    
    Point xu(dim);
    xu[0]=1.3;
    xu[1]=2.0;
    xu[2]=15;
    
    set_bounds ( xl , xu );
    
    set_bbit (2 , INTEGER );
    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "constrained" );
}


bool SpringMOD_Mixed_Case1::eval_x ( Eval_Point          & x          ,
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

