#include <string>
#include "Rastrigin_Mixed_Case221.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=20, m=1                                */
/*       Rastrigin Mixed Case 22                  */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rastrigin_Mixed_Case221::Rastrigin_Mixed_Case221 ( void )
: Problem ( "Rastrigin_Mixed_Case221" , "Rastrigin_Mixed_Case221" , "xe.txt" , 20 , 1 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );

    int dim = 20;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/Rastrigin_Mixed_Case221/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int k=0; k<nbpoints; k++)
    {
        fich >> temp2 >> temp;
        for (int j=0; j<4; j++){
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
        for (int j=4; j<20; j++){
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
    xl[3]=0;
    xl[10]=-5;
    xl[11]=-5;
    xl[12]=-5.0;
    xl[13]=-5.0;
    xl[14]=-5.0;
    xl[15]=-5.0;
    xl[16]=-5.0;
    xl[17]=-5.0;
    xl[18]=-5.0;
    xl[19]=-5.0;
    
    Point xu(dim);
    xu[0]=6;
    xu[1]=6;
    xu[2]=6;
    xu[3]=6;
    xu[10]=5;
    xu[11]=5;
    xu[12]=5.0;
    xu[13]=5.0;
    xu[14]=5.0;
    xu[15]=5.0;
    xu[16]=5.0;
    xu[17]=5.0;
    xu[18]=5.0;
    xu[19]=5.0;
    
    set_bounds ( xl , xu );
    
    set_bbit(0 , INTEGER);
    set_bbit(1 , INTEGER);
    set_bbit(2 , INTEGER);
    set_bbit(3 , INTEGER);
    set_bbit(4 , CATEGORICAL);
    set_bbit(5 , CATEGORICAL);
    set_bbit(6 , CATEGORICAL);
    set_bbit(7 , CATEGORICAL);
    set_bbit(8 , CATEGORICAL);
    set_bbit(9 , CATEGORICAL);
    set_bbit(10 , INTEGER);
    set_bbit(11 , INTEGER);
    
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "categorical" );
}


bool Rastrigin_Mixed_Case221::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    
    double f;
    double pi = 3.1415926535897932385;
    int n=20;
    vector<double> y(20);
    for (int k=0; k<20; k++){
        y[k]=x[k].value();
    }
    
    for (int k=0; k<4; k++) {
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
void EP_Rastrigin_Mixed_Case221::construct_extended_points ( const Eval_Point & x )
{
    for (int k=4; k<8; k++){
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
    for (int k=8; k<9; k++){
        int cur = static_cast<int> (x[k].value());
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
            y[k] = other_types[j];	
            add_extended_poll_point ( y , *(x.get_signature())  );
        }
    }
}


