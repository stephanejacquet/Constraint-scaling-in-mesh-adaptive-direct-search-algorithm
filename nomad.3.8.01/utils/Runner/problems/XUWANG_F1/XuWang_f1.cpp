#include "XuWang_f1.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f1::XuWang_f1 ( void )
  : Problem ( "XUWANG_F1" , "XUWANG_F1" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0   ( NOMAD::Point ( 2 , 0.0 ) );
  set_f_lb ( -850 );

  set_bounds ( NOMAD::Point ( 2 , -500 ) , NOMAD::Point ( 2 , 500 ) );

  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f1::eval_x ( NOMAD::Eval_Point & xx         ,
		       bool              & count_eval   ) const {

  double x = xx[0].value() , y = xx[1].value() ,
         z = -x*sin(sqrt(fabs(x))) - y*sin(sqrt(fabs(y)));

  xx.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
