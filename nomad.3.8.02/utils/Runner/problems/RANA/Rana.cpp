#include "Rana.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Rana::Rana ( void )
  : Problem ( "RANA" , "RANA" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0   ( NOMAD::Point ( 2 , 0.0 ) );
  set_f_lb ( -550 );

  set_bounds ( NOMAD::Point ( 2 , -512.0 ) , NOMAD::Point ( 2 , 512.0 ) );

  add_keyword ( "published"    );
  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Rana::eval_x ( NOMAD::Eval_Point & x          ,
		    bool              & count_eval   ) const {

  NOMAD::Double z = 
    x[0]*sin(sqrt(fabs(x[1].value()+1-x[0].value())))
    *cos(sqrt(fabs(x[0].value()+x[1].value()+1)))
    + (x[1]+1)*cos(sqrt(fabs(x[1].value()+1-x[0].value())))
    *sin(sqrt(fabs(x[0].value()+x[1].value()+1)));

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
