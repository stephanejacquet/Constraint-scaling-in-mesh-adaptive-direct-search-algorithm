#include "Tridia.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Tridia::Tridia ( int n )
  : Problem ( "TRIDIA" + NOMAD::itos(n)          ,
	      "TRIDIA"                           ,
	      "xe_"    + NOMAD::itos(n) + ".txt" ,
	      n                                  ,
	      1                                    ) {

  set_bbot ( 0, NOMAD::OBJ            );
  set_x0   ( NOMAD::Point ( n , 1.0 ) );
  set_f_lb ( 0.0                      );

  add_keyword ( "smooth"    );
  add_keyword ( "quadratic" );
  add_keyword ( "published" );

  if ( n == 10 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  ); 
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Tridia::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  int    n = get_n();
  double f = 0.0;

  for ( int i = 2 ; i <= n ; ++i ) 
    f += i * pow ( 2*x[i-1].value()-x[i-2].value() , 2 );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
