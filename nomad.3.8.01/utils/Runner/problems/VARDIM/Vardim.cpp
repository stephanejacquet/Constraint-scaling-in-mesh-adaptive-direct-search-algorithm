#include "Vardim.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Vardim::Vardim ( int n )
  : Problem ( "VARDIM" + NOMAD::itos(n)          ,
	      "VARDIM"                           ,
	      "xe_"    + NOMAD::itos(n) + ".txt" ,
	      n                                  ,
	      1                                    ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point  x0 ( n );
  NOMAD::Double d = 1.0 / n;
  
  x0[n-1] = 0.0;
  
  int i = n-2;
  while ( i >= 0 ) {
    x0[i] = x0[i+1] + d;
    --i;
  }

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
bool Vardim::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  int    n = get_n() , i;
  double f = 0.0;

  for ( i = 0 ; i < n ; ++i )
    f += pow ( x[i].value()-1 , 2 );

  double fi = 0.0;
  for ( i = 1 ; i <= n ; ++i )
    fi += i * (x[i-1].value()-1);

  f += pow ( fi , 2 );
  f += pow ( fi , 4 );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
