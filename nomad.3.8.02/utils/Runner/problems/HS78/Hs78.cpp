#include "Hs78.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Hs78::Hs78 ( void )
  : Problem ( "HS78" , "HS78" , "xe.txt" , 5 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 5 );

  x0[0] = -2.0;
  x0[1] =  1.5;
  x0[2] =  2.0;
  x0[3] = -1.0;
  x0[4] = -1.0;

  set_x0   ( x0   );
  set_f_lb ( -3.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Hs78::eval_x ( NOMAD::Eval_Point & x          ,
		    bool              & count_eval   ) const {

  double f1 = x[0].value()*x[0].value() + x[1].value()*x[1].value()
    + x[2].value()*x[2].value() + x[3].value()*x[3].value()
    + x[4].value()*x[4].value()
    - 10.0;

  double f2 = x[1].value()*x[2].value()-5*x[3].value()*x[4].value();

  double f3 = x[0].value()*x[0].value()*x[0].value()
    + x[1].value()*x[1].value()*x[1].value()+1;

  double f = x[0].value()*x[1].value()*x[2].value()*x[3].value()*x[4].value()
    + 10*(fabs(f1)+fabs(f2)+fabs(f3));


  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
