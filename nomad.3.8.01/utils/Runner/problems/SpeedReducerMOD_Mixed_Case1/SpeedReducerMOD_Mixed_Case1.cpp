#include <string>
#include "SpeedReducerMOD_Mixed_Case1.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=7, m=12                                */
/*       SpeedReducerMOD Mixed Case 1             */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
SpeedReducerMOD_Mixed_Case1::SpeedReducerMOD_Mixed_Case1 ( void )
: Problem ( "SpeedReducerMOD_Mixed_Case1" , "SpeedReducerMOD_Mixed_Case1" , "xe.txt" , 7 , 12 ) {
    
    
    set_bbot ( 0 , NOMAD::OBJ );
    set_bbot ( 1 , NOMAD::PB );
    set_bbot ( 2 , NOMAD::PB );
    set_bbot ( 3 , NOMAD::PB );
    set_bbot ( 4 , NOMAD::PB );
    set_bbot ( 5 , NOMAD::PB );
    set_bbot ( 6 , NOMAD::PB );
    set_bbot ( 7 , NOMAD::PB );
    set_bbot ( 8 , NOMAD::PB );
    set_bbot ( 9 , NOMAD::PB );
    set_bbot ( 10 , NOMAD::PB );
    set_bbot ( 11 , NOMAD::PB );

    int dim = 7;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/SpeedReducerMOD_Mixed_Case1/DOE.plan");
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
        fich >> x0[4];
        fich >> x0[5];
        fich >> x0[6];
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=2.6;
    xl[1]=0.7;
    xl[2]=17;
    xl[3]=7.3;
    xl[4]=7.3;
    xl[5]=2.9;
    xl[6]=5.0;

    
    Point xu(dim);
    xu[0]=3.6;
    xu[1]=0.8;
    xu[2]=28;
    xu[3]=8.3;
    xu[4]=8.3;
    xu[5]=3.9;
    xu[6]=5.5;
    
    set_bounds ( xl , xu );
    
    set_bbit (2 , INTEGER );
    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "constrained" );
}


bool SpeedReducerMOD_Mixed_Case1::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    double x1, x2, x3, x4, x5, x6, x7;
    double f;
    double A, B, C, D, A1, A2;
    double g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11;
    
    x1=x[0].value();
    x2=x[1].value();
    x3=x[2].value();
    x4=x[3].value();
    x5=x[4].value();
    x6=x[5].value();
    x7=x[6].value();
    
    // Cost function
    A = 3.3333 * x3 * x3 + 14.9334 * x3 - 43.0934;
    B = x6 * x6 + x7 * x7;
    C = x6 * x6 * x6 + x7 * x7 * x7;
    D = x4 * x6 * x6 + x5 * x7 * x7;
    f = 0.7854 * x1 * x2 * x2 * A - 1.508 * x1 * B + 7.477 * C + 0.7854 * D;
    
    // Constraints
    A1 = std::sqrt(745.0 * x4 * 745.0 * x4 + 16900000.0 * x2 * x2 * x3 * x3);
    A2 = std::sqrt(745.0 * x5 * 745.0 * x5 + 157500000.0 * x2 * x2 * x3 * x3);
    g1 = 27.0 - x1 * x2 * x2 * x3;                                // <= 0
    g2 = 397.5 - x1 * x2 * x2 * x3 * x3;                          // <= 0
    g3 = 1.93 * x4 * x4 * x4 - x2 * x6 * x6 * x6 * x6 * x3;       // <= 0
    g4 = 1.93 * x5 * x5 * x5 - x2 * x7 * x7 * x7 * x7 * x3;       // <= 0
    g5 = A1 - 110.0 * x2 * x3 * x6 * x6 * x6;                     // <= 0
    g6 = A2 - 85.0 * x2 * x3 * x7 * x7 * x7;                      // <= 0
    g7 = x2 * x3 - 40.0;                                          // <= 0
    g8 = 5.0 * x2 - x1;                                           // <= 0
    g9 = x1 - 12 * x2;                                            // <= 0
    g10 = 1.9 + 1.5 * x6 - x4;                                    // <= 0
    g11 = 1.9 + 1.1 * x7 - x5;                                    // <= 0
    
    count_eval = true; // count a black-box evaluation
    
    x.set_bb_output  ( 0 , f  ); // objective value
    x.set_bb_output  ( 1 , g1  );
    x.set_bb_output  ( 2 , g2  );
    x.set_bb_output  ( 3 , g3  );
    x.set_bb_output  ( 4 , g4  );
    x.set_bb_output  ( 5 , g5  );
    x.set_bb_output  ( 6 , g6  );
    x.set_bb_output  ( 7 , g7  );
    x.set_bb_output  ( 8 , g8  );
    x.set_bb_output  ( 9 , g9  );
    x.set_bb_output  ( 10 , g10  );
    x.set_bb_output  ( 11 , g11  );
    
    return true;       // the evaluation succeeded
}

