#include "Pbc1.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Pbc1::Pbc1 ( void )
  : Problem ( "PBC1" , "PBC1" , "xe.txt" , 5 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 5 , 1.0 );

  x0[0] =  0.0;
  x0[1] = -1.0;
  set_x0   ( x0  );

  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Pbc1::eval_x ( NOMAD::Eval_Point & x          ,
		    bool              & count_eval   ) const {

  double ti , fi , z = -1e+20;

  count_eval = true;

  // scaling :
  double x2 = x[2].value() * 10.0;
  double x4 = x[4].value() * 10.0;

  for ( int i = 1 ; i <= 30 ; i++ ) {
    ti = -1+2*(i-1)/29.0;
    fi = (x[0].value()+x[1].value()*ti+x2*ti*ti)/
      (1+x[3].value()*ti+x4*ti*ti)
         - (sqrt(1+(8*ti-1)*(8*ti-1))*atan(8*ti))/(8*ti);

    if ( fabs(fi) > z )
      z = fabs(fi);
    
    if ( z > 1e+20 ) {
      x.set_bb_output ( 0 , 1e+20 );
      return true;
    }
  }

  x.set_bb_output ( 0 , z );
  
  return true;
}
