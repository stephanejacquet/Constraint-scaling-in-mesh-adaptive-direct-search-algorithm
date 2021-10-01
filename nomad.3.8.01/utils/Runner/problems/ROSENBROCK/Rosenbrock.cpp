#include "Rosenbrock.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rosenbrock::Rosenbrock ( void )
  : Problem ( "ROSENBROCK" , "ROSENBROCK" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0   ( NOMAD::Point ( 2 , 0.0 ) );
  set_f_lb ( 0.0 );

  set_bounds ( NOMAD::Point ( 2 , -5.12 ) , NOMAD::Point ( 2 , 5.12 ) );

  add_keyword ( "published"    );
  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Rosenbrock::eval_x ( NOMAD::Eval_Point & x          ,
			  bool              & count_eval   ) const {

  x.set_bb_output ( 0 , 100*pow(x[1].value()-x[0].value()*x[0].value(),2)
		    +pow(1-x[0].value(),2) );

  count_eval = true;
  
  return true;
}
