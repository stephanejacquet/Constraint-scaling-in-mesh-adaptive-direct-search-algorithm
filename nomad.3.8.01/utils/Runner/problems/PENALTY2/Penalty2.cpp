#include "Penalty2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Penalty2::Penalty2 ( int n )
  : Problem ( "PENALTY2" + NOMAD::itos(n)          ,
	      "PENALTY2"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      1                                      ) {

  set_bbot ( 0, NOMAD::OBJ            );
  set_x0   ( NOMAD::Point ( n , 0.5 ) );
  set_f_lb ( 0.0                      );

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
bool Penalty2::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  const double pa = sqrt(1e-5);

  int i , n = get_n();

  double fi = x[0].value() - 0.2;
  double yi;
  double f  = pow ( fi , 2 );

  for ( i = 2 ; i <= n ; ++i ) {
    yi  = exp ( i / 10.0 ) + exp ( (i-1) / 10.0 );
    fi  = pa * ( exp(x[i-1].value()/10.0) + exp(x[i-2].value()/10.0) - yi );
    f  += pow ( fi , 2 );
  }

  double em01 = exp(-0.1);

  for ( i = n+1 ; i < 2*n ; ++i ) {
    fi  = pa * ( exp(x[i-n].value()/10.0) - em01 );
    f  += pow ( fi , 2 );
  }


  fi = 0.0;
  for ( i = 1 ; i <= n ; ++i ) {
    fi += (n-i+1)*pow(x[i-1].value(),2);
  }
  fi -= 1.0;
  f += pow ( fi , 2 );

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
