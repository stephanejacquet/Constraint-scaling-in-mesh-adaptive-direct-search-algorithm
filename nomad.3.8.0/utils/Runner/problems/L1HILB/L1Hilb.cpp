#include "L1Hilb.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
L1Hilb::L1Hilb ( void )
  : Problem ( "L1HILB" , "L1HILB" , "xe.txt" , 50 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ             );
  set_x0   ( NOMAD::Point ( 50 , 1.0 ) );
  set_f_lb ( 0.0                       );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool L1Hilb::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

 
  double s , z = 0.0;
  int    i , j;

  for ( i = 1 ; i <= 50 ; i++ ) {
    s = 0.0;
    for ( j = 1 ; j <= 50 ; j++ )
      s += x[j-1].value() / (i+j-1);
    z += fabs(s);
  }

  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
