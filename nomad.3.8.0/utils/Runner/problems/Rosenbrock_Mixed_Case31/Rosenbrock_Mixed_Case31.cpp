#include <string>
#include "Rosenbrock_Mixed_Case31.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=1                                 */
/*       Rosenbrock Mixed Case 31                 */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rosenbrock_Mixed_Case31::Rosenbrock_Mixed_Case31 ( void )
: Problem ( "Rosenbrock_Mixed_Case31" , "Rosenbrock_Mixed_Case31" , "xe.txt" , 2 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 2;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rosenbrock_Mixed_Case31/DOE.plan");
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
    xl[1]=-2.0;
    
    Point xu(dim);
    xu[1]=2.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , CATEGORICAL );
    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "categorical");
}


bool Rosenbrock_Mixed_Case31::eval_x ( Eval_Point          & x          ,
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


/*--------------------------------------*/
/*  construct the extended poll points  */
/*      (categorical neighborhoods)     */
/*--------------------------------------*/
void EP_Rosenbrock_Mixed_Case31::construct_extended_points ( const Eval_Point & x )
{
    int cur = static_cast<int> (x[0].value());
    
    vector<int> other_types;
    switch ( cur )
    {
        case 0:
            other_types.push_back(1);
            other_types.push_back(2);
            break;
        case 1:
            other_types.push_back(0);
            other_types.push_back(2);
            break;
        case 2:
            other_types.push_back(0);
            other_types.push_back(1);
            break;
    }
    
    for ( size_t k = 0 ; k < other_types.size() ; k++ )
    {
        Point y = x ;
        y[0] = other_types[k];
        add_extended_poll_point ( y , *(x.get_signature())  );
    }
}

