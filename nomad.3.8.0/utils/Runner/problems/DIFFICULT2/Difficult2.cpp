#include "Difficult2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Difficult2::Difficult2 ( void )
  : Problem ( "DIFFICULT2" , "DIFFICULT2" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0   ( NOMAD::Point ( 2 , 0.0 ) );

  set_f_lb ( -0.0002 );

  set_bounds ( NOMAD::Point ( 2 , -100 ) , NOMAD::Point ( 2 , 100 ) );

  add_keyword ( "mads_dfo_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Difficult2::eval_x ( NOMAD::Eval_Point & x          ,
			  bool              & count_eval   ) const {

  x.set_bb_output ( 0 , (x[0]-x[1]).abs() - 1e-6 * (x[0]+x[1]) );

  count_eval = true;
  
  return true;
}
