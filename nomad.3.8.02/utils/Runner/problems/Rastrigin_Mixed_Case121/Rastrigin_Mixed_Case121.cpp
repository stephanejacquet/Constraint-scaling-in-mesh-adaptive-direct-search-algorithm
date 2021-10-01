#include <string>
#include "Rastrigin_Mixed_Case121.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=1                                 */
/*       Rastrigin Mixed Case 12                  */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rastrigin_Mixed_Case121::Rastrigin_Mixed_Case121 ( void )
: Problem ( "Rastrigin_Mixed_Case121" , "Rastrigin_Mixed_Case121" , "xe.txt" , 10 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 10;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rastrigin_Mixed_Case121/DOE.plan");
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
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=0;
    xl[1]=0;
    xl[2]=0;
    xl[6]=-5.0;
    xl[7]=-5.0;
    xl[8]=-5;
    xl[9]=-5;
    
    Point xu(dim);
    xu[0]=6;
    xu[1]=6;
    xu[2]=3;
    xu[6]=5.0;
    xu[7]=5.0;
    xu[8]=5;
    xu[9]=5;
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    set_bbit (1 , INTEGER );
    set_bbit (2 , INTEGER );
    set_bbit (3 , CATEGORICAL );
    set_bbit (4 , CATEGORICAL );
    set_bbit (5 , CATEGORICAL );
    set_bbit (8 , INTEGER );
    set_bbit (9 , INTEGER );
    
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
}


bool Rastrigin_Mixed_Case121::eval_x ( Eval_Point          & x          ,
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

/*--------------------------------------*/
/*  construct the extended poll points  */
/*      (categorical neighborhoods)     */
/*--------------------------------------*/
void EP_Rastrigin_Mixed_Case121::construct_extended_points ( const Eval_Point & x )
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


