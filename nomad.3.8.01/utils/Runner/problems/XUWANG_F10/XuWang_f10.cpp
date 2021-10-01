#include "XuWang_f10.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f10::XuWang_f10 ( void )
  : Problem ( "XUWANG_F10" , "XUWANG_F10" , "xe.txt" , 2 , 3 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );

  NOMAD::Point x0 ( 2 );
  x0[0] =  56.5;
  x0[1] =  50.0;
  set_x0   ( x0  );

  set_f_lb ( -7000.0 );

  NOMAD::Point lb ( 2 );
  lb[0] = 13;
  lb[1] = 0;

  set_bounds ( lb , NOMAD::Point ( 2 , 100 ) );

  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f10::eval_x ( NOMAD::Eval_Point & xx         ,
			  bool              & count_eval   ) const {

  double x = xx[0].value() , y = xx[1].value();

  double z  =  pow ( x-10 , 3 ) + pow ( y-20 , 3 );
  double g1 = -pow ( x-5 , 2 ) - pow (y-5 , 2 ) + 100;
  double g2 =  pow ( x-6 , 2 ) + pow ( y-5 , 2 ) - 82.81;

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );

  count_eval = true;
  
  return true;
}
