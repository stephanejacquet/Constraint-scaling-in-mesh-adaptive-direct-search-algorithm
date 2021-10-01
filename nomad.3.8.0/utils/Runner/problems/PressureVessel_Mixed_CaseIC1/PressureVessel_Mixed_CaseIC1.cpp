#include <string>
#include "PressureVessel_Mixed_CaseIC1.hpp"

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
PressureVessel_Mixed_CaseIC1::PressureVessel_Mixed_CaseIC1 ( void )
: Problem ( "PressureVessel_Mixed_CaseIC1" , "PressureVessel_Mixed_CaseIC1" , "xe.txt" , 4 , 4 ) {
    
    
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
    ifstream fich("problems/PressureVessel_Mixed_CaseIC1/DOE.plan");
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
    xl[2]=10.0;
    xl[3]=10.0;
    
    Point xu(dim);
    xu[2]=200.0;
    xu[3]=200.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , CATEGORICAL );
    set_bbit (1 , CATEGORICAL );
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "categorical" );
}


bool PressureVessel_Mixed_CaseIC1::eval_x ( Eval_Point          & x          ,
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


/*--------------------------------------*/
/*  construct the extended poll points  */
/*      (categorical neighborhoods)     */
/*--------------------------------------*/
void EP_PressureVessel_Mixed_CaseIC1::construct_extended_points ( const Eval_Point & x )
{
    vector<int> other_types1;
    vector<int> other_types2;
    
    for (int j=1; j<100; j++)
    {
        if (x[0].value() != j)
        {
            other_types1.push_back(j);
        }
    }
    for ( size_t k = 0 ; k < other_types1.size() ; k++ )
    {
        Point y = x ;
        y[0] = other_types1[k];
        add_extended_poll_point ( y , *(x.get_signature())  );
    }
    
    for (int j=1; j<100; j++)
    {
        if (x[1].value() != j)
        {
            other_types2.push_back(j);
        }
    }
    for ( size_t k = 0 ; k < other_types2.size() ; k++ )
    {
        Point y = x ;
        y[1] = other_types2[k];
        add_extended_poll_point ( y , *(x.get_signature())  );
    }
}

