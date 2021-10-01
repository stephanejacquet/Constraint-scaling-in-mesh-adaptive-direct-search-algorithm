#include "Pentagon.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Pentagon::Pentagon ( void )
  : Problem ( "PENTAGON" , "PENTAGON" , "xe.txt" , 6 , 16 ) {

  set_bbot (  0 , NOMAD::PB );
  set_bbot (  1 , NOMAD::PB );
  set_bbot (  2 , NOMAD::PB );
  set_bbot (  3 , NOMAD::PB );
  set_bbot (  4 , NOMAD::PB );
  set_bbot (  5 , NOMAD::PB );
  set_bbot (  6 , NOMAD::PB );
  set_bbot (  7 , NOMAD::PB );
  set_bbot (  8 , NOMAD::PB );
  set_bbot (  9 , NOMAD::PB );
  set_bbot ( 10 , NOMAD::PB );
  set_bbot ( 11 , NOMAD::PB );
  set_bbot ( 12 , NOMAD::PB );
  set_bbot ( 13 , NOMAD::PB );
  set_bbot ( 14 , NOMAD::PB );
  set_bbot ( 15 , NOMAD::OBJ );

  NOMAD::Point x0 ( 6 , 0.0 );

  set_x0   ( x0  );
  set_f_lb ( -2.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Pentagon::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {
  
  const double PI = 3.14159265359;

  int i , j , k = 0;

  double g [15];

  for ( i = 0 ; i <= 4 ; i+=2 )
    for ( j = 0 ; j <= 4 ; j++ )
      g[k++] = x[i].value()*cos(2.0*PI*j/5.0)
	+ x[i+1].value()*sin(2.0*PI*j/5.0) - 1.0;
  
  double f1 = -sqrt( pow(x[0].value()-x[2].value(),2)
		     + pow(x[1].value()-x[3].value(),2) );
  double f2 = -sqrt( pow(x[2].value()-x[4].value(),2)
		     + pow(x[3].value()-x[5].value(),2) );
  double f3 = -sqrt( pow(x[4].value()-x[0].value(),2)
		     + pow(x[5].value()-x[1].value(),2) );

  double z = ( f1 > f2 ) ? f1 : f2;
  if ( f3 > z )
    z = f3;

  for ( i = 0 ; i < 15 ; ++i )
    x.set_bb_output ( i , g[i] );

  x.set_bb_output ( 15 , z );

  count_eval = true;
  
  return true;
}
