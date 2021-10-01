#include "Styrene.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Styrene::Styrene ( void ) 
  : Problem ( "STYRENE" , "STYRENE" , "xe.txt" , 8 , 12 ) {

  set_bbot (  0 , NOMAD::EB  );
  set_bbot (  1 , NOMAD::EB  );
  set_bbot (  2 , NOMAD::EB  );
  set_bbot (  3 , NOMAD::EB  );
  set_bbot (  4 , NOMAD::PB  );
  set_bbot (  5 , NOMAD::PB  );
  set_bbot (  6 , NOMAD::PB  );
  set_bbot (  7 , NOMAD::PB  );
  set_bbot (  8 , NOMAD::PB  );
  set_bbot (  9 , NOMAD::PB  );
  set_bbot ( 10 , NOMAD::PB  );
  set_bbot ( 11 , NOMAD::OBJ );

  set_bounds ( NOMAD::Point ( 8 , 0.0 ) , NOMAD::Point ( 8 , 100.0 ) );

  NOMAD::Point x0 ( 8 );
  x0[0] = 54;
  x0[1] = 66;
  x0[2] = 86;
  x0[3] =  8;
  x0[4] = 29;
  x0[5] = 51;
  x0[6] = 32;
  x0[7] = 15;
  set_x0 ( x0 );

  set_f_lb ( -3.5e+07 );

  set_bb_exe ( "problems/STYRENE/truth_" + ENV + ".exe" );

  add_keyword ( "published"        );
  add_keyword ( "real_application" );
  add_keyword ( "orthomads_paper"  );
  add_keyword ( "mads_dfo_paper"   );
}
