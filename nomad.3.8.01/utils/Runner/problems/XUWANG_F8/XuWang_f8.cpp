#include "XuWang_f8.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f8::XuWang_f8 ( void )
  : Problem ( "XUWANG_F8" , "XUWANG_F8" , "xe.txt" , 5 , 7 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );
  set_bbot ( 3, NOMAD::PB  );
  set_bbot ( 4, NOMAD::PB  );
  set_bbot ( 5, NOMAD::PB  );
  set_bbot ( 6, NOMAD::PB  );

  NOMAD::Point x0 ( 5 , 36.0 );

  x0[0] =  90.0;
  x0[1] =  39.0;

  set_x0   ( x0  );

  set_f_lb ( -31000 );

  NOMAD::Point lb ( 5 , 27.0 );
  lb[0] = 78.0;
  lb[1] = 33.0;
  NOMAD::Point ub ( 5 , 45.0 );
  ub[0] = 102.0;

  set_bounds ( lb , ub );

  add_keyword ( "XuWang_paper" );
  add_keyword ( "quadratic"    );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f8::eval_x ( NOMAD::Eval_Point & xx          ,
			 bool              & count_eval   ) const {

  double x[5];

  for ( int i = 0 ; i < 5 ; ++i )
    x[i] = xx[i].value();

  double z = 5.3578547*x[2]*x[2] + 0.8356891*x[0]*x[4]
    + 37.293239*x[0] - 40792.141;

  double g1 = -85.334407 - 0.0056858*x[1]*x[4] - 0.0006262*x[0]*x[3]
    + 0.0022053*x[2]*x[4];
  double g2 = 85.334407 + 0.0056858*x[1]*x[4] + 0.0006262*x[0]*x[3]
    - 0.0022053*x[2]*x[4] - 92;
  double g3 = -80.51249 - 0.0071317*x[1]*x[4] - 0.0029955*x[0]*x[1]
    - 0.0021813*x[2]*x[2] + 90;
  double g4 = 80.51249 + 0.0071317*x[1]*x[4] + 0.0029955*x[0]*x[1]
    + 0.0021813*x[2]*x[2] - 110;
  double g5 = -9.300961 -0.0047026*x[2]*x[4]
    - 0.0012547*x[0]*x[2] - 0.0019085*x[2]*x[3] + 20;
  double g6 = 9.300961 +0.0047026*x[2]*x[4]
    + 0.0012547*x[0]*x[2] + 0.0019085*x[2]*x[3] - 25;

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
