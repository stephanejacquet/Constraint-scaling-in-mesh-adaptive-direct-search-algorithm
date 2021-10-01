#include "Trefethen.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Trefethen::Trefethen ( void )
  : Problem ( "TREFETHEN" , "TREFETHEN" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0 ( NOMAD::Point ( 2 , 3.0 ) );

  set_f_lb ( -3.4 );

  set_bounds ( NOMAD::Point ( 2 , -5.0 ) , NOMAD::Point ( 2 , 5.0 ) );

  add_keyword ( "published" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Trefethen::eval_x ( NOMAD::Eval_Point & xx         ,
			 bool              & count_eval   ) const {

  double x = xx[0].value();
  double y = xx[1].value();

  xx.set_bb_output ( 0 , (x*x+y*y)/4 + exp(sin(50*x)) - sin(10*(x+y))
		     + sin(60*exp(y))
		     + sin(70*sin(x)) + sin(sin(80*y)) );

  count_eval = true;
  
  return true;
}
