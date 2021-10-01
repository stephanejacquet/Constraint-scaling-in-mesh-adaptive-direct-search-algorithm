#include "Goffin.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Goffin::Goffin ( void )
  : Problem ( "GOFFIN" , "GOFFIN" , "xe.txt" , 50 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 50 );

  x0[ 0] = -24.5;
  x0[ 1] = -23.5;
  x0[ 2] = -22.5;
  x0[ 3] = -21.5;
  x0[ 4] = -20.5;
  x0[ 5] = -19.5;
  x0[ 6] = -18.2;
  x0[ 7] = -17.5;
  x0[ 8] = -16.5;
  x0[ 9] = -15.5;
  x0[10] = -14.5;
  x0[11] = -13.5;
  x0[12] = -12.5;
  x0[13] = -11.5;
  x0[14] = -10.5;
  x0[15] = -9.5;
  x0[16] = -8.5;
  x0[17] = -7.5;
  x0[18] = -6.5;
  x0[19] = -5.5;
  x0[20] = -4.5;
  x0[21] = -3.5;
  x0[22] = -2.5;
  x0[23] = -1.5;
  x0[24] = -0.5;
  x0[25] =  0.5;
  x0[26] =  1.5;
  x0[27] =  2.5;
  x0[28] =  3.5;
  x0[29] =  4.5;
  x0[30] =  5.5;
  x0[31] =  6.5;
  x0[32] =  7.5;
  x0[33] =  8.5;
  x0[34] =  9.5;
  x0[35] = 10.5;
  x0[36] = 11.5;
  x0[37] = 12.5;
  x0[38] = 13.5;
  x0[39] = 14.5;
  x0[40] = 15.5;
  x0[41] = 16.5;
  x0[42] = 17.5;
  x0[43] = 18.5;
  x0[44] = 19.5;
  x0[45] = 20.5;
  x0[46] = 21.5;
  x0[47] = 22.5;
  x0[48] = 23.5;
  x0[49] = 24.5;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Goffin::eval_x ( NOMAD::Eval_Point & x          ,
		      bool              & count_eval   ) const {

  count_eval = true;

  double max = -1e+20 , sum = 0.0;
  for ( int i = 0 ; i < 50 ; i++ ) {
    if ( x[i].value() > max )
      max = x[i].value();
    sum += x[i].value();

    if ( sum > 1e+20 ) {
      x.set_bb_output ( 0 , 1e+20 );
      return true;
      break;
    }
  }

  x.set_bb_output ( 0 , 50 * max - sum );
  
  return true;
}
