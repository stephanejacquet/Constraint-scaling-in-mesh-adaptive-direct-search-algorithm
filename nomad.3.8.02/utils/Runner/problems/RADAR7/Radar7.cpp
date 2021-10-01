#include "Radar7.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Radar7::Radar7 ( void )
  : Problem ( "RADAR7" , "RADAR7" , "xe.txt" , 7 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ                    );
  set_x0   ( NOMAD::Point ( 7 , 3.141592654 ) );
  set_f_lb ( 0.0                              );

  set_bounds ( NOMAD::Point ( 7 , 0.0 ) , NOMAD::Point ( 7 , 6.283185308 ) );

  add_keyword ( "published" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Radar7::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  double f = -1e20 , g , h;
  int    i , j , k;

  for ( i = 1 ; i <= 7 ; ++i ) {

    g = 0.0;

    for ( j = i ; j <= 7 ; ++j ) {
      h = 0.0;
      for ( k = abs(2*i-j-1)+1 ; k <= j ; ++k )
	h += x[k-1].value();
      g += cos(h);
    }
    if ( g > f )
      f = g;
    if ( -g > f )
      f = -g;
  }
      
  for ( i = 1 ; i < 7 ; ++i ) {
    g = 0.5;
    for ( j = i+1 ; j <= 7 ; ++j ) {
      h = 0.0;
      for ( k = abs(2*i-j)+1 ; k <= j ; ++k )
	h += x[k-1].value();
      g += cos(h);
    }
	
    if ( g > f )
      f = g;
    
    if ( -g > f )
      f = -g;
  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
