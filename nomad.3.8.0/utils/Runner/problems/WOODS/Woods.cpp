#include "Woods.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Woods::Woods ( int n )
  : Problem ( "WOODS" + NOMAD::itos(n)          ,
	      "WOODS"                           ,
	      "xe_"   + NOMAD::itos(n) + ".txt" ,
	      n                                 ,
	      1                                   ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( n , -1.0 );

  int i , n2 = n/2;
  for ( i = 0 ; i < n2 ; ++i )
    x0[2*i] = -3.0;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "smooth"    );
  add_keyword ( "published" );

  if ( n == 12 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Woods::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  int    n4 = get_n() / 4 , i , i4;
  double f = 0.0;

  for ( i = 1 ; i <= n4 ; ++i ) {

    i4 = 4*i;

    f += 100 * pow ( pow ( x[i4-4].value(), 2) - x[i4-3].value() , 2 )
      + pow ( x[i4-4].value() - 1 , 2 )
      + 90 * pow ( pow(x[i4-2].value(),2)-x[i4-1].value() , 2 )
      + pow ( 1-x[i4-2].value() , 2 )
      + 10.1 * ( pow(x[i4-3].value()-1,2)+pow(x[i4-1].value()-1,2) )
      + 19.8 * (x[i4-3].value()-1) * (x[i4-1].value()-1);
  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
