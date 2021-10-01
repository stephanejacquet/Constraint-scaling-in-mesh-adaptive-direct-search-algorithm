#include "Crescent.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Crescent::Crescent ( int n )
  : Problem ( "CRESCENT" + NOMAD::itos(n)          ,
	      "CRESCENT"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      3                                      ) {

  set_bbot ( 0, NOMAD::OBJ );
  set_bbot ( 1, NOMAD::PB  );
  set_bbot ( 2, NOMAD::PB  );

  NOMAD::Point x0 ( n , 0.0 );
  x0[0] = n;

  set_x0   ( x0 );

  if ( n == 10 )
    set_f_lb ( -10 );

  add_keyword ( "published" );
  add_keyword ( "quadratic" );

  if ( n == 10 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Crescent::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  int n = get_n() , n2 = n*n;
  double z = 0.0 , g1 = 0.0 , g2 = 0.0;

  for ( int i = 0 ; i < n ; i++ ) {
    z = x[i].value();
    g1 += (z-1)*(z-1);
    g2 += (z+1)*(z+1);
  }

  x.set_bb_output ( 0 , z     );
  x.set_bb_output ( 1 , g1-n2 );
  x.set_bb_output ( 2 , n2-g2 );

  count_eval = true;
  
  return true;
}
