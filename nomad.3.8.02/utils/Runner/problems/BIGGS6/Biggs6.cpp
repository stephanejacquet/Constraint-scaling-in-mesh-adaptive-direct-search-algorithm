#include "Biggs6.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Biggs6::Biggs6 ( void )
  : Problem ( "BIGGS6" , "BIGGS6" , "xe.txt" , 6 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 6 , 1.0 );
  x0[1] = 2.0;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "smooth"          );
  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Biggs6::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {
  double f = 0.0;

  for ( int i = 1 ; i <= 13 ; ++i ) {
    
    f += pow ( -exp(-0.1*i)+5*exp(-i)-3*exp(-0.4*i)
	       + x[2].value()*exp(-0.1*i*x[0].value()) -
	       x[3].value()*exp(-0.1*i*x[1].value()) +
	       x[5].value()*exp(-0.1*i*x[4].value()) , 2 );

  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
