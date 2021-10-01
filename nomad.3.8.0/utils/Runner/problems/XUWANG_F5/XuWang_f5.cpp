#include "XuWang_f5.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f5::XuWang_f5 ( void )
  : Problem ( "XUWANG_F5" , "XUWANG_F5" , "xe.txt" , 13 , 10 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );
  set_bbot ( 3, NOMAD::PB  );
  set_bbot ( 4, NOMAD::PB  );
  set_bbot ( 5, NOMAD::PB  );
  set_bbot ( 6, NOMAD::PB  );
  set_bbot ( 7, NOMAD::PB  );
  set_bbot ( 8, NOMAD::PB  );
  set_bbot ( 9, NOMAD::PB  );
  
  NOMAD::Point x0 ( 13 , 0.5 );
  x0[ 9] = 50.0;
  x0[10] = 50.0;
  x0[11] = 50.0;

  set_x0   ( x0  );
  set_f_lb ( -15 );

  NOMAD::Point ub ( 13 , 1 );
  ub[ 9] = 100.0;
  ub[10] = 100.0;
  ub[11] = 100.0;

  set_bounds ( NOMAD::Point ( 13 , 0 ) , ub );

  add_keyword ( "XuWang_paper" );
  add_keyword ( "quadratic"    );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f5::eval_x ( NOMAD::Eval_Point & xx         ,
			 bool              & count_eval   ) const {

  double x[13];

  for ( int i = 0 ; i < 13 ; ++i )
    x[i] = xx[i].value();

  double z = 5*(x[0]+x[1]+x[2]+x[3])-5*(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]+x[3]*x[3])
    -x[4]-x[5]-x[6]-x[7]-x[8]-x[9]-x[10]-x[11]-x[12];

  double g1 = 2*x[0]+2*x[1]+x[9]+x[10]-10;
  double g2 = 2*x[0]+2*x[2]+x[9]+x[11]-10;
  double g3 = 2*x[1]+2*x[2]+x[10]+x[11]-10;
  double g4 = x[9]-8*x[0];
  double g5 = x[10]-8*x[1];
  double g6 = x[11]-8*x[2];
  double g7 = -2*x[3]-x[4]+x[9];
  double g8 = -2*x[5]-x[6]+x[10];
  double g9 = -8*x[2]-x[8]+x[11];

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );
  xx.set_bb_output ( 3 , g3 );
  xx.set_bb_output ( 4 , g4 );
  xx.set_bb_output ( 5 , g5 );
  xx.set_bb_output ( 6 , g6 );
  xx.set_bb_output ( 7 , g7 );
  xx.set_bb_output ( 8 , g8 );
  xx.set_bb_output ( 9 , g9 );

  count_eval = true;
  
  return true;
}
