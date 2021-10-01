#include "XuWang_f13.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f13::XuWang_f13 ( void )
  : Problem ( "XUWANG_F13" , "XUWANG_F13" , "xe.txt" , 7 , 5 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );
  set_bbot ( 3, NOMAD::PB  );
  set_bbot ( 4, NOMAD::PB  );

  set_x0   ( NOMAD::Point ( 7 , 0.0 ) );
  set_f_lb ( 650 );

  set_bounds ( NOMAD::Point ( 7 , -10.0 ) , NOMAD::Point ( 7 , 10.0 )  );

  add_keyword ( "XuWang_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f13::eval_x ( NOMAD::Eval_Point & xx         ,
			  bool              & count_eval   ) const {

  double x[7];

  for ( int i = 0 ; i < 7 ; ++i )
    x[i] = xx[i].value();

  double z = pow(x[0]-10,2) + 5*pow(x[1]-12,2)+pow(x[2],4)+3*pow(x[3]-11,2)
    +10*pow(x[4],6)+7*x[5]*x[5]+pow(x[6],4)-4*x[5]*x[6]-10*x[5]-8*x[6];
  double g1 = -127+2*x[0]*x[0]+3*pow(x[1],4)+x[2]+4*pow(x[3],2)+5*x[4];
  double g2 = -282+7*x[0]+3*x[1]+10*x[2]*x[2]+x[3]-x[4];
  double g3 = -196+23*x[0]+x[1]*x[1]+6*x[5]*x[5]-8*x[6];
  double g4 = 4*x[0]*x[0]+x[1]*x[1]-3*x[0]*x[1]+2*x[2]*x[2]+5*x[5]-11*x[6];

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );
  xx.set_bb_output ( 3 , g3 );
  xx.set_bb_output ( 4 , g4 );

  count_eval = true;
  
  return true;
}
