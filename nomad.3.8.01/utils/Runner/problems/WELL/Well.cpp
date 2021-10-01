#include "Well.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Well::Well ( void ) 
  : Problem ( "WELL" , "WELL" , "xe.txt" , 17 , 13 ) {

  set_bbit (  0 , NOMAD::INTEGER );
  set_bbit (  1 , NOMAD::INTEGER );
  set_bbit (  2 , NOMAD::INTEGER );
  set_bbit (  3 , NOMAD::INTEGER );
  set_bbit (  4 , NOMAD::INTEGER );
  set_bbit (  5 , NOMAD::INTEGER );
  set_bbit (  6 , NOMAD::INTEGER );
  set_bbit (  7 , NOMAD::INTEGER );
  set_bbit (  8 , NOMAD::INTEGER );
  set_bbit (  9 , NOMAD::INTEGER );
  set_bbit ( 10 , NOMAD::INTEGER );
  set_bbit ( 11 , NOMAD::INTEGER );

  set_bbot (  0 , NOMAD::PB  );
  set_bbot (  1 , NOMAD::PB  );
  set_bbot (  2 , NOMAD::PB  );
  set_bbot (  3 , NOMAD::PB  );
  set_bbot (  4 , NOMAD::PB  );
  set_bbot (  5 , NOMAD::PB  );
  set_bbot (  6 , NOMAD::PB  );
  set_bbot (  7 , NOMAD::PB  );
  set_bbot (  8 , NOMAD::PB  );
  set_bbot (  9 , NOMAD::PB  );
  set_bbot ( 10 , NOMAD::PB  );
  set_bbot ( 11 , NOMAD::PB  );
  set_bbot ( 12 , NOMAD::OBJ );

  NOMAD::Point x0 ( 17 );
  x0[ 0] = 48;
  x0[ 1] = 21;
  x0[ 2] = 44;
  x0[ 3] =  9;
  x0[ 4] = 39;
  x0[ 5] = 35;
  x0[ 6] = 33;
  x0[ 7] = 23;
  x0[ 8] = 26;
  x0[ 9] = 23;
  x0[10] = 14;
  x0[11] = 12;
  x0[12] = -0.005333;
  x0[13] = -0.005333;
  x0[14] = -0.005333;
  x0[15] = -0.005333;
  x0[16] = -0.005333;
  set_x0 ( x0 );

  set_f_lb ( 0.139e+06 );

  NOMAD::Point lb ( 17 , -0.0063996 );
  lb[ 0] = 10;
  lb[ 1] = 1;
  lb[ 2] = 10;
  lb[ 3] = 1;
  lb[ 4] = 10;
  lb[ 5] = 1;
  lb[ 6] = 10;
  lb[ 7] = 1;
  lb[ 8] = 10;
  lb[ 9] = 1;
  lb[10] = 10;
  lb[11] = 1;

  NOMAD::Point ub ( 17 , 0.002 );
  ub[ 0] = 49;
  ub[ 1] = 40;
  ub[ 2] = 49;
  ub[ 3] = 40;
  ub[ 4] = 49;
  ub[ 5] = 40;
  ub[ 6] = 49;
  ub[ 7] = 40;
  ub[ 8] = 49;
  ub[ 9] = 40;
  ub[10] = 49;
  ub[11] = 40;

  set_bounds ( lb , ub );

  set_bb_exe ( "problems/WELL/bb_runner_" + ENV + ".exe" );

  add_keyword ( "published"        );
  add_keyword ( "real_application" );
}
