#include "Srosenbr.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Srosenbr::Srosenbr ( int n )
  : Problem ( "SROSENBR" + NOMAD::itos(n)          ,
	      "SROSENBR"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      1                                      ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( n , 1.0 );
  for ( int i = 0 ; i < n/2 ; ++i )
    x0[2*i] = 1.2;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "smooth"    );
  add_keyword ( "published" );

  if ( n == 10 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Srosenbr::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {
  double f = 0.0;
  int    n2 = get_n()/2 , i , i2;

  for ( i = 1 ; i <= n2 ; ++i ) {
    i2 = 2*i;
    f += pow ( 10 * (x[i2-1].value() - pow(x[i2-2].value(),2) ) , 2 );
    f += pow ( 1 - x[i2-2].value() , 2 );
  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
