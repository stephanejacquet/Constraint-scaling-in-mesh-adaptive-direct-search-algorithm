#include "Wong1.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Wong1::Wong1 ( void )
  : Problem ( "WONG1" , "WONG1" , "xe.txt" , 7 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ            );
  set_x0   ( NOMAD::Point ( 7 , 1.0 ) );
  set_f_lb ( 0.0                      );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Wong1::eval_x ( NOMAD::Eval_Point & x          ,
		     bool              & count_eval   ) const {

  double f1 = (x[0].value()-10)*(x[0].value()-10)+5*(x[1].value()-12)
    *(x[1].value()-12)+pow(x[2].value(),4)+
    3*(x[3].value()-11)*(x[3].value()-11) + 10*pow(x[4].value(),6)
    + 7*x[5].value()*x[5].value() +
    pow(x[6].value(),4) - 4*x[5].value()*x[6].value() - 10*x[5].value()
    - 8*x[6].value();

  double z = f1;

  double f = f1 + 10*(2*x[0].value()*x[0].value()
		      +3*pow(x[1].value(),4)+x[2].value()+4
		      *x[3].value()*x[3].value()
		      +5*x[4].value()-127);
  
  if ( f > z )
    z = f;

  f = f1 + 10*(7*x[0].value()+3*x[1].value()+10*x[2].value()*x[2].value()
	       +x[3].value()-x[4].value()-282);
  if ( f > z )
    z = f;

  f = f1 + 10*(23*x[0].value()+x[1].value()*x[1].value()+6
	       *x[5].value()*x[5].value()-8*x[6].value()-196);
  if ( f > z )
    z = f;

  f = f1 + 10*(4*x[0].value()*x[0].value()+x[1].value()*x[1].value()-3
	       *x[0].value()*x[1].value()
	       +2*x[2].value()*x[2].value()
	       +5*x[5].value()-11*x[6].value());
  if ( f > z )
    z = f;
  
  if ( z > 1e+20 )
    z = 1e+20;

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
