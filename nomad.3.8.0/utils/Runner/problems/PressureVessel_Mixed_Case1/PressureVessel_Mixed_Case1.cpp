#include <string>
#include "PressureVessel_Mixed_Case1.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=4, m=4                                 */
/*       PressureVessel Mixed Case 1              */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
PressureVessel_Mixed_Case1::PressureVessel_Mixed_Case1 ( void )
: Problem ( "PressureVessel_Mixed_Case1" , "PressureVessel_Mixed_Case1" , "xe.txt" , 4 , 4 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );
    set_bbot ( 1 , NOMAD::PB );
    set_bbot ( 2 , NOMAD::PB );
    set_bbot ( 3 , NOMAD::PB );

    
    int dim = 4;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/PressureVessel_Mixed_Case1/DOE.plan");
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
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=1;
    xl[1]=1;
    xl[2]=10.0;
    xl[3]=10.0;
    
    Point xu(dim);
    xu[0]=99;
    xu[1]=99;
    xu[2]=200.0;
    xu[3]=200.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    set_bbit (1 , INTEGER );
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "PressureVessel_Mixed_Case1" );
}


bool PressureVessel_Mixed_Case1::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    
    double f, g1, g2, g3,x1,x2, x3, x4;
    const double g_Pi = 3.14159265358979323846;
    
    x1=x[0].value();
    x2=x[1].value();
    x3=x[2].value();
    x4=x[3].value();
    
    // Cost function
    f = 0.6224 * 0.0625 * x1 * x3 * x4 + 1.7781 * 0.0625 * x2 * x3 * x3 + 3.1661 * 0.0625 * 0.0625 * x1 * x1 * x4 + 19.84 * 0.0625 * 0.0625 * x1 * x1 * x3;
    
    // Constraints
    g1 = - 0.0625 * x1 + 0.0193 * x3;   // <= 0
    g2 = - 0.0625 * x2 + 0.00954 * x3;  // <= 0
    g3 = - g_Pi * x3 * x3 * x4 - 4.0 / 3.0 * g_Pi * x3 * x3 * x3 + 1296000.0;  // <= 0
    
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  );
    x.set_bb_output  ( 2 , g2  );
    x.set_bb_output  ( 3 , g3  );
    
    return true;       // the evaluation succeeded
}

