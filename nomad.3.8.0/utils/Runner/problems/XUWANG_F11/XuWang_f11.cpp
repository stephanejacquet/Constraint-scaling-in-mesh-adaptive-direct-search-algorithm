#include "XuWang_f11.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f11::XuWang_f11 ( void )
  : Problem ( "XUWANG_F11" , "XUWANG_F11" , "xe.txt" , 10 , 9 ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );
  set_bbot ( 3, NOMAD::PB  );
  set_bbot ( 4, NOMAD::PB  );
  set_bbot ( 5, NOMAD::PB  );
  set_bbot ( 6, NOMAD::PB  );
  set_bbot ( 7, NOMAD::PB  );
  set_bbot ( 8, NOMAD::PB  );

  set_x0   ( NOMAD::Point ( 10 , 0.0 ) );
  set_f_lb ( 25 );

  set_bounds ( NOMAD::Point ( 10 , -10.0 ) , NOMAD::Point ( 10 , 10.0 ) );

  add_keyword ( "XuWang_paper" );
  add_keyword ( "quadratic"    );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f11::eval_x ( NOMAD::Eval_Point & xx         ,
			  bool              & count_eval   ) const {

  double x[10];

  for ( int i = 0 ; i < 10 ; ++i )
    x[i] = xx[i].value();

  double z = x[0]*x[0]+x[1]*x[1]+x[0]*x[1]-14*x[0]-16*x[1]
    +pow(x[2]-10,2)+4*pow(x[3]-5,2)
    +pow(x[4]-3,2)+2*pow(x[5]-1,2)+5*x[6]*x[6]+7*pow(x[7]-11,2)
    +2*pow(x[8]-10,2)
    +pow(x[9]-7,2)+45;

  double g1 = -105+4*x[0]+5*x[1]-3*x[6]+9*x[7];
  double g2 = 10*x[0]-8*x[1]-17*x[6]+2*x[7];
  double g3 = -8*x[0]+2*x[1]+5*x[8]-2*x[9]-12;
  double g4 = 3*pow(x[0]-2,2)+4*pow(x[1]-3,2)+2*x[2]*x[2]-7*x[3]-120;
  double g5 = 5*x[0]*x[0]+8*x[1]+pow(x[2]-6,2)-2*x[3]-40;
  double g6 = x[0]*x[0]+2*pow(x[1]-2,2)-2*x[0]*x[1]+14*x[4]-x[5];
  double g7 = 0.5*pow(x[0]-8,2)+2*pow(x[1]-4,2)+3*x[4]*x[4]-x[5]-30;
  double g8 = -3*x[0]+6*x[1]+12*pow(x[8]-8,2)-7*x[9];

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output ( 1 , g1 );
  xx.set_bb_output ( 2 , g2 );
  xx.set_bb_output ( 3 , g3 );
  xx.set_bb_output ( 4 , g4 );
  xx.set_bb_output ( 5 , g5 );
  xx.set_bb_output ( 6 , g6 );
  xx.set_bb_output ( 7 , g7 );
  xx.set_bb_output ( 8 , g8 );

  count_eval = true;
  
  return true;
}
