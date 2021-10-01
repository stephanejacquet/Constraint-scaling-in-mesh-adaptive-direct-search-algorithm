#include "Branin.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Branin::Branin ( void )
  : Problem ( "BRANIN" , "BRANIN" , "xe.txt" , 2 , 1 ) {

  NOMAD::Point x0 ( 2 ) , lb ( 2 ) , ub ( 2 );
  x0[0] = 2.5;
  x0[1] = 7.5;

  set_bbot ( 0, NOMAD::OBJ );
  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  lb[0] = -5;
  lb[1] =  0;
  
  ub[0] = 10;
  ub[1] = 15;

  set_bounds ( lb , ub );

  add_keyword ( "published" );
  add_keyword ( "mads_dfo_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Branin::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {
  x.set_bb_output ( 0 , pow ( x[1].value() - 0.1291845091*pow(x[0].value(),2)
			      + 1.591549431*x[0].value()-6, 2 )
		    + 9.602112642*cos(x[0].value())+10 );
  count_eval = true;
  return true;
}
