#include "XuWang_f3.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f3::XuWang_f3 ( void )
  : Problem ( "XUWANG_F3" , "XUWANG_F3" , "xe.txt" , 2 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0   ( NOMAD::Point ( 2 , 0.0 ) );

  set_f_lb ( 0.0 );

  set_bounds ( NOMAD::Point ( 2 , -600 ) , NOMAD::Point ( 2 , 600 ) );

  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f3::eval_x ( NOMAD::Eval_Point & xx         ,
			 bool              & count_eval   ) const {

  double x = xx[0].value() , y = xx[1].value() ,
    z = (pow(x-100,2)+pow(y-100,2))/4000
	- cos(x-100) * cos((y-100)/sqrt(2)) + 1;

  xx.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
