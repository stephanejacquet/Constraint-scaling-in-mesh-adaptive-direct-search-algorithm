#include "XuWang_f7.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
XuWang_f7::XuWang_f7 ( int n )
  : Problem ( "XUWANG_F7" + NOMAD::itos(n)          ,
	      "XUWANG_F7"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      1                                      ) {

  set_bbot ( 0, NOMAD::OBJ );

  set_x0 ( NOMAD::Point ( n , 0.0 ) );

  if ( n == 9 ) {
    set_f_lb ( -1 );
    add_keyword ( "XuWang_paper" );
  }

  set_bounds ( NOMAD::Point ( n , 0.0 ) , NOMAD::Point ( n , 1.0 ) );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool XuWang_f7::eval_x ( NOMAD::Eval_Point & x          ,
			 bool              & count_eval   ) const {

  int nm1 = get_n() , n = nm1+1;
  const double C =  pow ( sqrt(n) , n );

  double tmp = 1.0;
  double z   = 1.0;

  for ( int i = 0 ; i < nm1 ; ++i ) {
    tmp -= x[i].value()*x[i].value();
    z *= x[i].value();
  }

  if ( tmp >= 0.0 )
    z *= -C * sqrt(tmp);
  else
    z = 1e20;

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
