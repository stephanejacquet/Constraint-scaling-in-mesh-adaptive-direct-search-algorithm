#include "Griewank.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Griewank::Griewank ( void )
  : Problem ( "GRIEWANK2" , "GRIEWANK2" , "xe.txt" , 2 , 1 ) {

  NOMAD::Point x0 ( 2 );
  x0[0] =  594;
  x0[1] = -551;

  set_bbot ( 0, NOMAD::OBJ );
  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  set_bounds ( NOMAD::Point ( 2 , -600 ) , NOMAD::Point ( 2 , 600 ) );

  add_keyword ( "published" );
  add_keyword ( "mads_dfo_paper" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Griewank::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {
  double sum = 0 , prod = 1;

  for ( int i = 0 ; i < 2 ; ++i ) {
    sum  += pow ( x[i].value() , 2 );
    prod *= cos(x[i].value()/sqrt(i+1.0));
  }
  x.set_bb_output ( 0 , sum/4000 - prod + 1 );
  count_eval = true;
  return true;
}
