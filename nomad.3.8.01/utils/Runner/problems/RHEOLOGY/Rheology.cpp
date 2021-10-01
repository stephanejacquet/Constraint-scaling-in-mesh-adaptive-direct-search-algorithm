#include "Rheology.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rheology::Rheology ( void )
  : Problem ( "RHEOLOGY" , "RHEOLOGY" , "xe.txt" , 3 , 1 ) {
  
  set_bbot ( 0, NOMAD::OBJ            );
  set_x0   ( NOMAD::Point ( 3 , 10.0 ) );
  set_f_lb ( 30.0 );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Rheology::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  double eta_0  = x[0].value() * 955.4;
  double lambda = x[1].value() * 38.5;
  double beta   = x[2].value() * 0.035;

  double f = 0.0;
  for ( int i = 0 ; i < 13 ; ++i )
    f += fabs ( ETA[i] -
		eta_0 *
		pow ( 1 + lambda*lambda*GAMMA[i]*GAMMA[i] ,
		      (beta-1)/2.0 ) );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
