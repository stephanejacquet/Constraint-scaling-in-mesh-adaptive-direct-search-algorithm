#include <string>
#include "SteppedCantileverBeamMOD1.hpp"

using namespace std;
using namespace NOMAD;

/*------------------------------------------------*/
/*               The problem                      */
/*------------------------------------------------*/
/*       n=10, m=12                               */
/*       SteppedCantileverBeamMOD 1               */
/*------------------------------------------------*/
/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
SteppedCantileverBeamMOD1::SteppedCantileverBeamMOD1 ( void )
: Problem ( "SteppedCantileverBeamMOD1" , "SteppedCantileverBeamMOD1" , "xe.txt" , 10 , 12 ) {
    
    
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

    int dim = 10;
    
    // TAKE ONLY THE FIRST POINT FROM DOE
    int nbpoints = 1;
    
    Point x0(dim); // starting points
    string temp;
    int temp2;
    ifstream fich("problems/SteppedCantileverBeamMOD1/DOE.plan");
    for (int k=0; k<7; k++)
    {
        getline(fich,temp);
    }
    for (int j=0; j<nbpoints; j++)
    {
        fich >> temp2 >> temp;
        fich >> x0[0];
        fich >> x0[1];
        if (x0[1]==45)
            x0[1]=0;
        else if (x0[1]==50)
            x0[1]=1;
        else if (x0[1]==55)
            x0[1]=2;
        else if (x0[1]==60)
            x0[1]=3;
        fich >> x0[2];
        if (x0[2]==2.4)
            x0[2]=0;
        else if (x0[2]==2.6)
            x0[2]=1;
        else if (x0[2]==2.8)
            x0[2]=2;
        else if (x0[2]==3.1)
            x0[2]=3;
        fich >> x0[3];
        if (x0[3]==45)
            x0[3]=0;
        else if (x0[3]==50)
            x0[3]=1;
        else if (x0[3]==55)
            x0[3]=2;
        else if (x0[3]==60)
            x0[3]=3;
        fich >> x0[4];
        if (x0[4]==2.4)
            x0[4]=0;
        else if (x0[4]==2.6)
            x0[4]=1;
        else if (x0[4]==2.8)
            x0[4]=2;
        else if (x0[4]==3.1)
            x0[4]=3;
        fich >> x0[5] >> x0[6] >> x0[7] >> x0[8] >> x0[9];
        getline(fich,temp);
        set_x0 ( x0 );
    }
    fich.close();
    
    Point xl(dim);
    xl[0]=1;
    xl[1]=0;
    xl[2]=0;
    xl[3]=0;
    xl[4]=0;
    xl[5]=30;
    xl[6]=1;
    xl[7]=30;
    xl[8]=1;
    xl[9]=30;

    Point xu(dim);
    xu[0]=5;
    xu[1]=3;
    xu[2]=3;
    xu[3]=3;
    xu[4]=3;
    xu[5]=65;
    xu[6]=5;
    xu[7]=65;
    xu[8]=5;
    xu[9]=65;
    
    set_bounds ( xl , xu );
    
    set_bbit (0 , INTEGER );
    set_bbit (1 , INTEGER );
    set_bbit (2 , INTEGER );
    set_bbit (3 , INTEGER );
    set_bbit (4 , INTEGER );
    set_bbit (5 , INTEGER );


    
    add_keyword ( "published"    );
    add_keyword ( "anne_sophie_crelot");
    add_keyword ( "constrained" );
}


bool SteppedCantileverBeamMOD1::eval_x ( Eval_Point          & x          ,
                                  bool                & count_eval   ) const
{
    Double f,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11;
    vector<Double> y(10);
    for (int k=0; k<10; k++){
        y[k]=x[k];
    }
    
    int l=100;
    int P=50000;
    double deltamax=2.7;
    double sigmamax=14000;
    int E=20000000;
    
    switch (static_cast<int>(x[1].value())) {
        case 0:
            y[1]=45;
            break;
        case 1:
            y[1]=50;
            break;
        case 2:
            y[1]=55;
            break;
        case 3:
            y[1]=60;
            break;
    }
    
    switch (static_cast<int>(x[2].value())) {
        case 0:
            y[2]=2.4;
            break;
        case 1:
            y[2]=2.6;
            break;
        case 2:
            y[2]=2.8;
            break;
        case 3:
            y[2]=3.1;
            break;
    }
    
    switch (static_cast<int>(x[3].value())) {
        case 0:
            y[3]=45;
            break;
        case 1:
            y[3]=50;
            break;
        case 2:
            y[3]=55;
            break;
        case 3:
            y[3]=60;
            break;
    }
    
    switch (static_cast<int>(x[4].value())) {
        case 0:
            y[4]=2.4;
            break;
        case 1:
            y[4]=2.6;
            break;
        case 2:
            y[4]=2.8;
            break;
        case 3:
            y[4]=3.1;
            break;
    }
    
    // Objective function
    f = l*(y[0]*y[1]+y[2]*y[3]+y[4]*y[5]+y[6]*y[7]+y[8]*y[9]) ;
    
    // Constraints
    g1 = (6*P*l)-sigmamax*y[8]*y[9]*y[9];
    g2 = (12*P*l)-sigmamax*y[6]*y[7]*y[7];
    g3 = (18*P*l)-sigmamax*y[4]*y[5]*y[5];
    g4 = (24*P*l)-sigmamax*y[2]*y[3]*y[3];
    g5 = (30*P*l)-sigmamax*y[0]*y[1]*y[1];
    g6 = ((P*l*l*l)/E)*(244*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+148*y[0]*y[1]*y[1]*y[1]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+76*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9]+28*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[8]*y[9]*y[9]*y[9]+4*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7])-deltamax*y[0]*y[1]*y[1]*y[1]*y[2]*y[3]*y[3]*y[3]*y[4]*y[5]*y[5]*y[5]*y[6]*y[7]*y[7]*y[7]*y[8]*y[9]*y[9]*y[9];
    g7 = y[1]-20*y[0];
    g8 = y[3]-20*y[2];
    g9 = y[5]-20*y[4];
    g10 = y[7]-20*y[6];
    g11 = y[9]-20*y[8];
    
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
    x.set_bb_output  ( 9 , g9  ); 
    x.set_bb_output  ( 10 , g10  ); 
    x.set_bb_output  ( 11 , g11  ); 
    
    return true;       // the evaluation succeeded
}

