#include "Bdqrtic.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Bdqrtic::Bdqrtic ( int n )
  : Problem ( "BDQRTIC" + NOMAD::itos(n)          ,
	      "BDQRTIC"                           ,
	      "xe_"     + NOMAD::itos(n) + ".txt" ,
	      n                                   ,
	      1                                     ) {

  set_bbot ( 0, NOMAD::OBJ             );
  set_x0   ( NOMAD::Point ( n , 1.0 )  );
  set_f_lb ( 0.0 );

  add_keyword ( "smooth"    );
  add_keyword ( "published" );

  if ( n == 10 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper" );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Bdqrtic::eval_x ( NOMAD::Eval_Point & x          ,
		       bool              & count_eval   ) const {
  int    n = get_n();
  double f = 0.0;

  for ( int i = 0 ; i < n-4 ; ++i ) {

    // Powell and Ana:

    // f += pow(pow(x[i],2) + 2*pow(x[i+1],2) + 3*pow(x[i+2],2) + 4*pow(x[i+3],2)
    // + 5*pow(x[N-1],2),2) - 4*x[i] + 3.0;

    f += pow(-4*x[i].value()+3,2) + pow(pow(x[i].value(),2)
      +  2*pow(x[i+1].value(),2)+3*pow(x[i+2].value(),2)
      +  4*pow(x[i+3].value(),2)+5*pow(x[n-1].value(),2),2);
  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
