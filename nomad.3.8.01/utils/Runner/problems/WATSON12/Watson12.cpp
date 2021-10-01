#include "Watson12.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Watson12::Watson12 ( void )
  : Problem ( "WATSON12" , "WATSON12" , "xe.txt" , 12 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 12 );
  x0[ 0] = -0.5;
  x0[ 1] =  0.5;
  x0[ 2] = -0.5;
  x0[ 3] =  0;
  x0[ 4] = -0.5;
  x0[ 5] =  0.5;
  x0[ 6] = -1;
  x0[ 7] =  5;
  x0[ 8] = -5;
  x0[ 9] =  5;
  x0[10] = -3;
  x0[11] =  0.5;

  set_x0 ( x0 );

  set_f_lb ( 0.0 );

  NOMAD::Point lb ( 12 );
  NOMAD::Point ub ( 12 );

  lb[ 0] = - 1;
  lb[ 1] =   0;
  lb[ 2] = - 1;
  lb[ 3] = - 1;
  lb[ 4] = - 1;
  lb[ 5] =   0;
  lb[ 6] = - 3;
  lb[ 7] =   0;
  lb[ 8] = -10;
  lb[ 9] =   0;
  lb[10] = - 5;
  lb[11] =   0;

  ub[ 0] =  0;
  ub[ 1] =  0.9;
  ub[ 2] =  0;
  ub[ 3] =  0.3;
  ub[ 4] =  0;
  ub[ 5] =  1;
  ub[ 6] =  0;
  ub[ 7] = 10;
  ub[ 8] =  0;
  ub[ 9] = 10;
  ub[10] =  0;
  ub[11] =  1;

  set_bounds ( lb , ub );

  add_keyword ( "published" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Watson12::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  double v2 = pow ( x[0].value() , 2 ) +
              pow ( x[1].value()-x[0].value()*x[0].value()-1 , 2 );
  double bk , ck , vk;
  int j;

  for ( int k = 1 ; k <= 29 ; ++k ) {
    
    bk = 0.0;
    for ( j = 1 ; j < 12 ; ++j )
      bk += j*x[j].value()*pow(k/29.0,j-1.0);
    
    ck = 0.0;
    for ( j = 1 ; j <= 12 ; ++j )
      ck += x[j-1].value()*pow(k/29.0,j-1.0);

    vk = bk-ck*ck-1.0;

    v2 += vk*vk;
  }

  x.set_bb_output ( 0 , v2 );

  count_eval = true;
  
  return true;
}
