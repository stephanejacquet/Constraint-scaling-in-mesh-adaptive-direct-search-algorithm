#include "Brownal.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Brownal::Brownal ( int n )
  : Problem ( "BROWNAL" + NOMAD::itos(n)          ,
	      "BROWNAL"                           ,
	      "xe_"     + NOMAD::itos(n) + ".txt" ,
	      n                                   ,
	      1                                     ) {

  set_bbot ( 0, NOMAD::OBJ             );
  set_x0   ( NOMAD::Point ( n , 0.5 )  );
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
bool Brownal::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {
  int    i , j , n = get_n();
  double f = 0.0 , fi;

  for ( i = 1 ; i < n ; ++i ) {
    fi = x[i-1].value() - n - 1;
    for ( j = 1 ; j <= n ; ++j ) {
      fi += x[j-1].value();
    }
    f += pow ( fi , 2 );
  }

  fi = 1.0;
  for ( i = 1 ; i <= n ; ++i )
    fi *= x[i-1].value();
  fi -= 1.0;
  f += pow ( fi , 2 );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
