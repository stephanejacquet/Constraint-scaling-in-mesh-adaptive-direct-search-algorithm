#include "Penalty1.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Penalty1::Penalty1 ( int n )
  : Problem ( "PENALTY1" + NOMAD::itos(n)          ,
	      "PENALTY1"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      1                                      ) {

  set_bbot ( 0, NOMAD::OBJ );
  
  NOMAD::Point x0 ( n );
  for ( int i = 0 ; i < n ; ++i )
    x0[i] = i+1;

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
bool Penalty1::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  const double pa = sqrt(1e-5);

  int    i , n = get_n();
  double f = 0.0 , fi;

  for ( i = 1 ; i <= n ; ++i ) {
    fi = pa * ( x[i-1].value() - 1 );
    f += pow ( fi , 2 );
  }
  
  fi = 0.0;
  for ( i = 1 ; i <= n ; ++i )
    fi += pow ( x[i-1].value() , 2 );
  fi -= 0.25;
  f += pow ( fi , 2 );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
