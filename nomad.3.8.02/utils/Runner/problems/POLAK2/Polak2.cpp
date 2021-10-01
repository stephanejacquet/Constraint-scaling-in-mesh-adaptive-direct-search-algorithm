#include "Polak2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Polak2::Polak2 ( void )
  : Problem ( "POLAK2" , "POLAK2" , "xe.txt" , 10 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ             );
  set_x0   ( NOMAD::Point ( 10 , 1.0 ) );
  set_f_lb ( 0.0                       );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Polak2::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  // scaling:
  double x0 = x[0].value() * 100.0;
  double x1 = x[1].value() / 10.0;
  double x2 = x[2].value() / 10.0;
  double x3 = x[3].value() / 10.0;
  double x4 = x[4].value() / 10.0;
  double x5 = x[5].value() / 10.0;
  double x6 = x[6].value() / 10.0;
  double x7 = x[7].value() / 10.0;
  double x8 = x[8].value() / 10.0;
  double x9 = x[9].value() / 10.0;

  double z = exp ( 1e-8*x0*x0+(x1+2)*(x1+2)+x2*x2+
		   4*x3*x3+x4*x4+x5*x5+
		   x6*x6+x7*x7+x8*x8+x9*x9);

  double f2 = exp ( 1e-8*x0*x0+(x1-2)*(x1-2)+x2*x2+
		    4*x3*x3+x4*x4+x5*x5+
		    x6*x6+x7*x7+x8*x8+x9*x9);
  
  if ( f2 > z )
    z = f2;

  if ( z > 1e+20 )
    z = 1e+20;

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
