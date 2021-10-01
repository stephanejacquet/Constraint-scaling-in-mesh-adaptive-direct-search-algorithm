#include "Filter.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Filter::Filter ( void )
  : Problem ( "FILTER" , "FILTER" , "xe.txt" , 9 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 9 );

  x0[0] =  0.0;
  x0[1] =  1.0;
  x0[2] =  0.0;
  x0[3] = -0.15;
  x0[4] =  0.0;
  x0[5] = -0.68;
  x0[6] =  0.0;
  x0[7] = -0.72;
  x0[8] =  0.37;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Filter::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  double ti , yi , vi , fi , z = -1e+20;

  for ( int i = 1 ; i <= 41 ; i++ ) {

    // ti :
    // ----
    if ( i <= 6 )
      ti = 0.01 * (i-1);
    else if ( i <= 20 )
      ti = 0.07 + 0.03 * (i-7);
    else if ( i == 21 )
      ti = 0.5;
    else if ( i <= 35 )
      ti = 0.54 + 0.03 * (i-22);
    else
      ti = 0.95 + 0.01 * (i-36);

    // yi and vi :
    // -----------
    yi = fabs(1-2*ti);
    vi = 3.14159265359L*ti;

    // fi :
    // ----
    fi = sqrt ( ( pow( x[0].value()+(1+x[1].value())*cos(vi) ,2)
		  + pow ( (1-x[1].value())*sin(vi) ,2) )
		/ ( pow(x[2].value()+(1+x[3].value())*cos(vi),2)
		    + pow((1-x[3].value())*sin(vi),2)) )
      * sqrt ( ( pow(x[4].value()+(1+x[5].value())*cos(vi),2)
		 + pow((1-x[5].value())*sin(vi),2) )
	       / ( pow(x[6].value()+(1+x[7].value())*cos(vi),2)
		   + pow((1-x[7].value())*sin(vi),2) ) ) * x[8].value()
      - yi;

    if ( fabs(fi) > z )
      z = fabs(fi);
  }


  x.set_bb_output ( 0 , z );

  count_eval = true;
  
  return true;
}
