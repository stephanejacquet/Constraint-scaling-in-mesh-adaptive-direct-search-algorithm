#include "XuWang_f12.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f12::XuWang_f12 ( void )
  : Problem ( "XUWANG_F12" , "XUWANG_F12" , "xe.txt" , 2 , 3 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );

  set_x0   ( NOMAD::Point ( 2 , 5.0 ) );
  set_f_lb ( -1.0 );

  set_bounds ( NOMAD::Point ( 2 , 0.0 ) , NOMAD::Point ( 2 , 10.0 ) );

  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f12::eval_x ( NOMAD::Eval_Point & xx         ,
			  bool              & count_eval   ) const {

  const double PI = 3.141592654;
  
  double x = xx[0].value() , y = xx[1].value();

  double z = -pow(sin(2*PI*x),3) * sin(2*PI*y) / ( (x+y)*pow(x,3) );
  double g1 = x*x-y+1;
  double g2 = 1-x+pow(y-4,2);

  xx.set_bb_output ( 0 , z );
  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );

  count_eval = true;
  
  return true;
}
