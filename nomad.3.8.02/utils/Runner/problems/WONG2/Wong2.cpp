#include "Wong2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Wong2::Wong2 ( void )
  : Problem ( "WONG2" , "WONG2" , "xe.txt" , 10 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 10 );

  x0[0] =  2.0;
  x0[1] =  3.0;
  x0[2] =  5.0;
  x0[3] =  5.0;
  x0[4] =  1.0;
  x0[5] =  2.0;
  x0[6] =  7.0;
  x0[7] =  3.0;
  x0[8] =  6.0;
  x0[9] = 10.0;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Wong2::eval_x ( NOMAD::Eval_Point & x          ,
		     bool              & count_eval   ) const {

  double f1 = x[0].value()*x[0].value()+x[1].value()*x[1].value()
    +x[0].value()*x[1].value()-14*x[0].value()-16*x[1].value()
    +pow(x[2].value()-10,2)+
    4*pow(x[3].value()-5,2)+
    pow(x[4].value()-3,2)+2*pow(x[5].value()-1,2)+5*x[6].value()*x[6].value()
    +7*pow(x[7].value()-11,2)+
    2*pow(x[8].value()-10,2)+pow(x[9].value()-7,2)+45;

  double z = f1;

  double f = f1 + 10*(3*pow(x[0].value()-2,2)+4*pow(x[1].value()-3,2)
		      +2*x[2].value()*x[2].value()-7*x[3].value()-120);
  if ( f > z )
    z = f;
  f = f1 + 10*(5*x[0].value()*x[0].value()
	       +8*x[1].value()+pow(x[2].value()-6,2)-2*x[3].value()-40);
  if ( f > z )
    z = f;
  f = f1 + 10*(0.5*pow(x[0].value()-8,2)+2*pow(x[1].value()-4,2)
	       +3*x[4].value()*x[4].value()-x[5].value()-30);
  if ( f > z )
    z = f;
  f = f1 + 10*(x[0].value()*x[0].value()+2*pow(x[1].value()-2,2)-2
	       *x[0].value()*x[1].value()+14*x[4].value()-6*x[5].value());
  if ( f > z )
    z = f;
  f = f1 + 10*(4*x[0].value()+5*x[1].value()-3*x[6].value()+9*x[7].value()-105);
  if ( f > z )
    z = f;
  f = f1 + 10*(10*x[0].value()-8*x[1].value()-17*x[6].value()+2*x[7].value());
  if ( f > z )
    z = f;
  f = f1 + 10*(-3*x[0].value()+6*x[1].value()+12*pow(x[8].value()-8,2)-7
	       *x[9].value());
  if ( f > z )
    z = f;
  f = f1 + 10*(-8*x[0].value()+2*x[1].value()+5*x[8].value()
	       -2*x[9].value()-12);
  if ( f > z )
    z = f;

  if ( z > 1e+20 )
    z = 1e+20;
  
  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
