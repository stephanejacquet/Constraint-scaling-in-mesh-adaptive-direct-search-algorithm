#include "Snake.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Snake::Snake ( void )
  : Problem ( "SNAKE" , "SNAKE" , "xe.txt" , 2 , 3 ) {

  set_bbot ( 0 , NOMAD::OBJ );
  set_bbot ( 1 , NOMAD::PB  );
  set_bbot ( 2 , NOMAD::PB  );
  
  NOMAD::Point x0 ( 2 , 0.0 );
  x0[1] = -10.0;
  set_x0 ( x0 );

  set_f_lb ( 0.0 );

  add_keyword ( "published" );
  add_keyword ( "mads_dfo_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Snake::eval_x ( NOMAD::Eval_Point & x          ,
		     bool              & count_eval   ) const {
  double x1 = x[0].value();
  double x2 = x[1].value();

  x.set_bb_output ( 0 , sqrt( (x1-20)*(x1-20) + (x2-1)*(x2-1) ) );
  x.set_bb_output ( 1 , sin(x1)-0.1-x2 );
  x.set_bb_output ( 2 , x2-sin(x1) );

  count_eval = true;
  
  return true;
}
