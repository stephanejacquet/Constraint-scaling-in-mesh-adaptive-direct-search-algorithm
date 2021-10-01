#include "ElAttar.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
ElAttar::ElAttar ( void )
  : Problem ( "ELATTAR" , "ELATTAR" , "xe.txt" , 6 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 6 );

  x0[0] =  2.0;
  x0[1] =  2.0;
  x0[2] =  7.0;
  x0[3] =  0.0;
  x0[4] = -2.0;
  x0[5] =  1.0;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool ElAttar::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {
  double ti , yi , z = 0.0;

  for ( int i = 1 ; i <= 51 ; i++ ) {
    ti = 0.1*(i-1);
    yi = 0.5*exp(-ti) - exp(-2*ti)  + 0.5*exp(-3*ti) +
      1.5*exp(-1.5*ti)*sin(7*ti) + exp(-2.5*ti)*sin(5*ti);
    z += fabs ( x[0].value() * exp(-x[1].value()*ti) *
		cos(x[2].value()*ti+x[3].value())
		+ x[4].value() * exp(-x[5].value()*ti) - yi );

    if ( z > 1e20 ) {
      z = 1e20;
      break;
    }
  }

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
