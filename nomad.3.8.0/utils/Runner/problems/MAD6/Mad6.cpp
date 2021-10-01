#include "Mad6.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Mad6::Mad6 ( void )
  : Problem ( "MAD6" , "MAD6" , "xe.txt" , 5 , 8 ) {

  set_bbot ( 0 , NOMAD::PB );
  set_bbot ( 1 , NOMAD::PB );
  set_bbot ( 2 , NOMAD::PB );
  set_bbot ( 3 , NOMAD::PB );
  set_bbot ( 4 , NOMAD::PB );
  set_bbot ( 5 , NOMAD::PB );
  set_bbot ( 6 , NOMAD::PB );
  set_bbot ( 7 , NOMAD::OBJ );

  NOMAD::Point x0 ( 5 );
  x0[0] = 0.5;
  x0[1] = 1.0;
  x0[2] = 1.5;
  x0[3] = 2.0;
  x0[4] = 2.5;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Mad6::eval_x ( NOMAD::Eval_Point & xx         ,
		    bool              & count_eval   ) const {

  const double PI = 3.14159265359;
  
  int    i , j;
  double x[7];

  for ( i = 0 ; i < 5 ; i++ )
    x[i] = xx[i].value();

  x[5] = 1+x[3];
  x[6] = 3.5;

  double g1 = -x[0]        + 0.4;
  double g2 =  x[0] - x[1] + 0.4;
  double g3 =  x[1] - x[2] + 0.4;
  double g4 =  x[2] - x[3] + 0.4;
  double g5 =  x[3] - x[4] + 0.4;
  double g6 =  x[4] - x[5] + 0.4;
  double g7 =  x[5] - x[6] + 0.4;

  double vi , fi , z = -1e20;

  for ( i = 1 ; i <= 163 ; ++i ) {

    vi = ( PI * (8.5+i*0.5) ) / 180.0;

    fi = 0.0;
    for ( j = 1 ; j <= 7 ; ++j )
      fi += cos ( 2.0 * PI * x[j-1] * sin(vi) );

    fi = ( 1 + 2 * fi ) / 15.0;

    if ( fabs(fi) > z )
      z = fabs(fi);
  }

  xx.set_bb_output ( 0 , g1 );
  xx.set_bb_output ( 1 , g2 );
  xx.set_bb_output ( 2 , g3 );
  xx.set_bb_output ( 3 , g4 );
  xx.set_bb_output ( 4 , g5 );
  xx.set_bb_output ( 5 , g6 );
  xx.set_bb_output ( 6 , g7 );
  xx.set_bb_output ( 7 , z  );
  count_eval = true;
  
  return true;
}
