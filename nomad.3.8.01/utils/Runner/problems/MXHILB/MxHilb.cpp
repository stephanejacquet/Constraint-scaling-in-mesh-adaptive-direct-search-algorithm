#include "MxHilb.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
MxHilb::MxHilb ( void )
  : Problem ( "MXHILB" , "MXHILB" , "xe.txt" , 50 , 1 ) {

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
bool MxHilb::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  count_eval = true;  

  int    i , j;
  double s , z = -1e+20;

  for ( i = 1 ; i <= 50 ; i++ ) {

    s = 0.0;
    for ( j = 1 ; j <= 50 ; j++ )
      s += x[j-1].value() / (i+j-1);
    s = fabs(s);
    
    if ( s > 1e+20 ) {
      x.set_bb_output ( 0 , 1e+20 );
      return true;
    }

    if ( s > z )
      z = s;
  }

  x.set_bb_output ( 0 , z );
  
  return true;
}
