#include <string>
#include "G07_Mixed_Case31.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=9                                */
/*       G07 Mixed Case 3                         */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
G07_Mixed_Case31::G07_Mixed_Case31 ( void )
: Problem ( "G07_Mixed_Case31" , "G07_Mixed_Case31" , "xe.txt" , 10 , 9 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );
    set_bbot ( 1 , NOMAD::PB );
    set_bbot ( 2 , NOMAD::PB );
    set_bbot ( 3 , NOMAD::PB );
    set_bbot ( 4 , NOMAD::PB );
    set_bbot ( 5 , NOMAD::PB );
    set_bbot ( 6 , NOMAD::PB );
    set_bbot ( 7 , NOMAD::PB );
    set_bbot ( 8 , NOMAD::PB );


    int dim = 10;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/G07_Mixed_Case31/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int j=0; j<nbpoints; j++)
    {
        fich >> temp2 >> temp;
        for (int k=0; k<6; k++)
        {
            fich >> x0[k];
            if (x0[k]==-10.0)
                x0[k]=0;
            else if (x0[k]==-5.0)
                x0[k]=1;
            else if (x0[k]==0.0)
                x0[k]=2;
            else if (x0[k]==1.3)
                x0[k]=3;
            else if (x0[k]==2.2)
                x0[k]=4;
            else if (x0[k]==5.0)
                x0[k]=5;
            else if (x0[k]==8.2)
                x0[k]=6;
            else if (x0[k]==8.7)
                x0[k]=7;
            else if (x0[k]==9.5)
                x0[k]=8;
            else
                x0[k]=9;
        }
        fich >> x0[6] >> x0[7] >> x0[8] >> x0[9];
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=0;
    xl[1]=0;
    xl[2]=0;
    xl[3]=0;
    xl[4]=0;
    xl[5]=0;
    xl[6]=-10;
    xl[7]=-10;
    xl[8]=-10;
    xl[9]=-10;
    
    Point xu(dim);
    xu[0]=9;
    xu[1]=9;
    xu[2]=9;
    xu[3]=9;
    xu[4]=9;
    xu[5]=9;
    xu[6]=10;
    xu[7]=10;
    xu[8]=10;
    xu[9]=10;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    set_bbit (1 , INTEGER );
    set_bbit (2 , INTEGER );
    set_bbit (3 , INTEGER );
    set_bbit (4 , INTEGER );
    set_bbit (5 , INTEGER );
    set_bbit (6 , INTEGER );
    set_bbit (7 , INTEGER );
    set_bbit (8 , INTEGER );
    set_bbit (9 , INTEGER );
    
    
    
    add_keyword ( "published"    );
    add_keyword ( "constrained" );
    add_keyword ( "anne_sophie_crelot");
}


bool G07_Mixed_Case31::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    Double f,g1,g2,g3,g4,g5,g6,g7,g8;
    
    vector<Double> y(10);
    for (int k=0; k<10; k++){
        y[k]=x[k];
    }
    for(int k=0; k<6; k++)
    {
        switch (static_cast<int>(x[k].value())) {
            case 0:
                y[k]=-10;
                break;
            case 1:
                y[k]=-5;
                break;
            case 2:
                y[k]=0;
                break;
            case 3:
                y[k]=1.3;
                break;
            case 4:
                y[k]=2.2;
                break;
            case 5:
                y[k]=5;
                break;
            case 6:
                y[k]=8.2;
                break;
            case 7:
                y[k]=8.7;
                break;
            case 8:
                y[k]=9.5;
                break;
            case 9:
                y[k]=10;
                break;
        }
    }
    
    // Objective function
    f = y[0] * y[0] + y[1] * y[1] + y[0] * y[1] - 14.0 * y[0] - 16.0 * y[1] + (y[2] - 10.0) * (y[2] - 10.0) + 4.0 * (y[3] - 5.0) * (y[3] - 5.0) + (y[4] - 3.0) * (y[4] - 3.0) + 2.0 * (y[5] - 1.0) * (y[5] - 1.0) + 5.0 * y[6] * y[6] + 7.0 * (y[7] - 11.0) * (y[7] - 11.0) + 2.0 * (y[8] - 10.0) * (y[8] - 10.0) +  (y[9] - 7.0) * (y[9] - 7.0) + 45.0 ;
    
    // Constraints
    
    g1 = -105.0 + 4 * y[0] + 5.0 * y[1] - 3.0 * y[6] + 9.0 * y[7];
    g2 = 10.0 * y[0] - 8.0 * y[1] - 17.0 * y[6] + 2.0 * y[7];
    g3 = -8.0 * y[0] + 2.0 * y[1] + 5.0 * y[8] - 2.0 * y[9] - 12.0;
    g4 = 3.0 * (y[0] - 2.0) * (y[0] - 2.0) + 4.0 * (y[1] - 3.0) * (y[1] - 3.0) + 2.0 * y[2] * y[2] - 7.0 * y[3] -120.0;
    g5 = 5.0 * y[0] * y[0] + 8.0 * y[1] + (y[2] - 6.0) * (y[2] - 6.0) - 2.0 * y[3] - 40.0;
    g6 = y[0] * y[0] + 2.0 * (y[1] - 2.0) * (y[1] - 2.0) - 2.0 * y[0] *  y[1] + 14.0 * y[4] - 6.0 * y[5];
    g7 = 0.5 * (y[0] - 8.0) * (y[0] - 8.0) + 2.0 * (y[1] - 4.0) * (y[1] - 4.0) + 3.0 * y[4] * y[4] - y[5] - 30.0;
    g8 = -3.0 * y[0] + 6.0 * y[1] + 12.0 * (y[8] - 8.0) * (y[8] - 8.0) - 7.0 * y[9];
    
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  ); // constraints value
    x.set_bb_output  ( 2 , g2  );
    x.set_bb_output  ( 3 , g3  ); 
    x.set_bb_output  ( 4 , g4  ); 
    x.set_bb_output  ( 5 , g5  ); 
    x.set_bb_output  ( 6 , g6  ); 
    x.set_bb_output  ( 7 , g7  ); 
    x.set_bb_output  ( 8 , g8  ); 
    
    return true;       // the evaluation succeeded
}

