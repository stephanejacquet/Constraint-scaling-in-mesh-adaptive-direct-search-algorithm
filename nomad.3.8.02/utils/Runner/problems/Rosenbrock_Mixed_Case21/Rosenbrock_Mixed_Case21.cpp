#include <string>
#include "Rosenbrock_Mixed_Case21.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=1                                 */
/*       Rosenbrock Mixed Case 21                 */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rosenbrock_Mixed_Case21::Rosenbrock_Mixed_Case21 ( void )
: Problem ( "Rosenbrock_Mixed_Case21" , "Rosenbrock_Mixed_Case21" , "xe.txt" , 2 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 2;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rosenbrock_Mixed_Case21/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int k=0; k<nbpoints; k++)
    {
        fich >> temp2 >> temp;
        for (int j=0; j<dim; j++)
        {
            fich >> x0[j];
        }
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=-2.0;
    xl[1]=-2.0;
    
    Point xu(dim);
    xu[0]=2.0;
    xu[1]=2.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
}


bool Rosenbrock_Mixed_Case21::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    
    NOMAD::Double f = 0.0;
    int sizeV=2;
    NOMAD::Double sizeVd=2.0;
    
    for ( int i = 0 ; i < sizeV-1 ; i++ ) {
        f=f+(1-x[i]).pow2()+100*((x[i+1]-x[i].pow2()).pow2());
    }
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    
    return true;       // the evaluation succeeded
}


