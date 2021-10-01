#include "G2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
G2::G2 ( int n )
  : Problem ( "G2" + NOMAD::itos(n)          ,
	      "G2"                           ,
	      "xe_"     + NOMAD::itos(n) + ".txt" ,
	      n                                   ,
	      3                                     ) {

  set_bbot ( 0, NOMAD::PB  );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::OBJ );

  set_bounds ( NOMAD::Point ( n , 0.0 ) , NOMAD::Point ( n , 10.0 ) );

  set_x0 ( NOMAD::Point ( n , 5.0 )  );

  add_keyword ( "published" );

  set_f_lb ( -1.0 );

  if ( n == 10 )
    add_keyword ( "XuWang_paper" );

  if ( n == 10 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool G2::eval_x ( NOMAD::Eval_Point & x          ,
		  bool              & count_eval   ) const {

  int    i , n = get_n();
  double z = 0.0;
 
  double sum1 = 0.0 , sum2 = 0.0 , sum3 = 0.0 , prod1 = 1.0 , prod2 = 1.0;
  
  for ( i = 0 ; i < n ; ++i ) {
    sum1  += pow ( cos(x[i].value()) , 4 );
    sum2  += x[i].value();
    sum3  += (i+1)*x[i].value()*x[i].value();
    prod1 *= pow ( cos(x[i].value()) , 2 );
    prod2 *= x[i].value();
  }

  double g1 = -prod2+0.75;
  double g2 = sum2 -7.5*n;

  if ( sum3 == 0.0 )
    z = 1e+20;
  else
    z  = - fabs ( ( sum1 - 2 * prod1 ) / sqrt(sum3) );


  x.set_bb_output ( 0 , g1 );
  x.set_bb_output ( 1 , g2 );
  x.set_bb_output ( 2 , z  );

  count_eval = true;
  
  return true;
}
