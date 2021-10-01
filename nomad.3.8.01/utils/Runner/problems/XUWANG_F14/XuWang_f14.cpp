#include "XuWang_f14.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f14::XuWang_f14 ( void )
  : Problem ( "XUWANG_F14" , "XUWANG_F14" , "xe.txt" , 8 , 7 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );
  set_bbot ( 3, NOMAD::PB  );
  set_bbot ( 4, NOMAD::PB  );
  set_bbot ( 5, NOMAD::PB  );
  set_bbot ( 6, NOMAD::PB  );

  NOMAD::Point x0 ( 8 , 505.0 );
  x0[0] = 5050;
  x0[1] = 5500;
  x0[2] = 5500;

  set_x0   ( x0  );

  set_f_lb ( 7000 );

  NOMAD::Point lb ( 8 , 10 );
  lb[0] =  100;
  lb[1] = 1000;
  lb[2] = 1000;

  NOMAD::Point ub ( 8 , 1000 );
  ub[0] = 10000;
  ub[1] = 10000;
  ub[2] = 10000;

  set_bounds ( lb , ub );

  add_keyword ( "XuWang_paper" );
  add_keyword ( "quadratic"    );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f14::eval_x ( NOMAD::Eval_Point & xx         ,
			  bool              & count_eval   ) const {
  double x[8];

  for ( int i = 0 ; i < 8 ; ++i )
    x[i] = xx[i].value();

  double z = x[0]+x[1]+x[2];

  double g1 = -1 + 0.0025 * (x[3]+x[5]);
  double g2 = -1 + 0.0025 * (x[4]+x[6]-x[3]);
  double g3 = -1 + 0.01   * (x[7]-x[4]);
  double g4 = -x[0]*x[5] + 833.33252*x[3] + 100*x[0] - 83333.333;
  double g5 = -x[1]*x[6] + 1250*x[4] + x[1]*x[3] - 1250*x[3];
  double g6 = -x[2]*x[7] + x[2]*x[4] - 2500*x[4] + 1250000;

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );
  xx.set_bb_output ( 3 , g3 );
  xx.set_bb_output ( 4 , g4 );
  xx.set_bb_output ( 5 , g5 );
  xx.set_bb_output ( 6 , g6 );

  count_eval = true;
  
  return true;
}
