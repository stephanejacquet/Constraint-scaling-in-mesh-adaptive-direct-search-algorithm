#include <string>
#include "Rastrigin_Mixed_Case31.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=1                                 */
/*       Rastrigin Mixed Case 3                     */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rastrigin_Mixed_Case31::Rastrigin_Mixed_Case31 ( void )
: Problem ( "Rastrigin_Mixed_Case31" , "Rastrigin_Mixed_Case31" , "xe.txt" , 2 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 2;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rastrigin_Mixed_Case31/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int k=0; k<nbpoints; k++)
    {
        fich >> temp2 >> temp;
        fich >> x0[0];
        fich >> x0[1];
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=-5;
    xl[1]=-5.0;
    
    Point xu(dim);
    xu[0]=5;
    xu[1]=5.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , CATEGORICAL );
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "categorical" );
}


bool Rastrigin_Mixed_Case31::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    
    double f,x1,x2;
    double pi = 3.1415926535897932385;
    int n=2;
    x1=x[0].value();
    x2=x[1].value();
    
    f = 10.0 * n + x1*x1-10.0*cos(2.0*pi*x1) + x2*x2-10.0*cos(2.0*pi*x2);
    
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    
    return true;       // the evaluation succeeded
}

/*--------------------------------------*/
/*  construct the extended poll points  */
/*      (categorical neighborhoods)     */
/*--------------------------------------*/
void EP_Rastrigin_Mixed_Case31::construct_extended_points ( const Eval_Point & x )
{
    int cur = static_cast<int> (x[0].value());
    
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
    
    for ( size_t k = 0 ; k < other_types.size() ; k++ )
    {
        Point y = x ;
        y[0] = other_types[k];	
        add_extended_poll_point ( y , *(x.get_signature())  );
    }
}
