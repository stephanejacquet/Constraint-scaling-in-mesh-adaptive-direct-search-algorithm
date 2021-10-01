#include <string>
#include "Rosenbrock_Mixed_Case11.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=2, m=1                                 */
/*       Rosenbrock Mixed Case 11                 */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rosenbrock_Mixed_Case11::Rosenbrock_Mixed_Case11 ( void )
: Problem ( "Rosenbrock_Mixed_Case11" , "Rosenbrock_Mixed_Case11" , "xe.txt" , 2 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 2;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rosenbrock_Mixed_Case11/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int k=0; k<nbpoints; k++)
    {
        fich >> temp2 >> temp;
        fich >> x0[0];
        if (x0[0].value()==-2)
            x0[0]=0;
        else if (x0[0].value()==-1.5)
            x0[0]=1;
        else if (x0[0].value()==-1)
            x0[0]=2;
        else if (x0[0].value()==-0.5)
            x0[0]=3;
        else if (x0[0].value()==0)
            x0[0]=4;
        else if (x0[0].value()==0.5)
            x0[0]=5;
        else if (x0[0].value()==1)
            x0[0]=6;
        else if (x0[0].value()==1.5)
            x0[0]=7;
        else if (x0[0].value()==2)
            x0[0]=8;
        fich >> x0[1];
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=0;
    xl[1]=-2.0;
    
    Point xu(dim);
    xu[0]=8;
    xu[1]=2.0;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
}


bool Rosenbrock_Mixed_Case11::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    
    Double f=0;
    int sizeV=2;
    vector<Double> y(2);
    for (int k=0; k<2; k++){
        y[k]=x[k];
    }
    
    switch (static_cast<int>(x[0].value())) {
        case 0:
            y[0]=-2;
            break;
        case 1:
            y[0]=-1.5;
            break;
        case 2:
            y[0]=-1;
            break;
        case 3:
            y[0]=-0.5;
            break;
        case 4:
            y[0]=0;
            break;
        case 5:
            y[0]=0.5;
            break;
        case 6:
            y[0]=1;
            break;
        case 7:
            y[0]=1.5;
            break;
        case 8:
            y[0]=2;
            break;
    }
    y[1]=x[1].value();
    
   	for ( int i = 0 ; i < sizeV-1 ; i++ ) {
        f=f+(1-y[i]).pow2()+100*((y[i+1]-y[i].pow2()).pow2());
    }
    
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    
    return true;       // the evaluation succeeded
}

