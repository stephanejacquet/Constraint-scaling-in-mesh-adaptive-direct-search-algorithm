#include "Hs114.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Hs114::Hs114 ( void )
  : Problem ( "HS114" , "HS114" , "xe.txt" , 9 , 7 ) {

  set_bbot ( 0 , NOMAD::PB  );
  set_bbot ( 1 , NOMAD::PB  );
  set_bbot ( 2 , NOMAD::PB  );
  set_bbot ( 3 , NOMAD::PB  );
  set_bbot ( 4 , NOMAD::PB  );
  set_bbot ( 5 , NOMAD::PB  );
  set_bbot ( 6 , NOMAD::OBJ );

  set_bounds ( NOMAD::Point ( 9 , 0.0 ) , NOMAD::Point ( 9 , 100.0 ) );

  NOMAD::Point x0 ( 9 );
  x0[0] = 74.9999999844;
  x0[1] = 91.6666659722;
  x0[2] = 60.9599999219;
  x0[3] = 98.6999999935;
  x0[4] = 52.5;
  x0[5] = 56.0;
  x0[6] = 55.5555555556;
  x0[7] = 85.7142857143;
  x0[8] = 0.0;

  set_x0   ( x0    );
  set_f_lb ( -1800 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Hs114::eval_x ( NOMAD::Eval_Point & x          ,
		     bool              & count_eval   ) const {

   // scaling :
  double  x2 = (x[0].value()/100.0)*(16000-1e-5) + 1e-5;
  double  x3 = (x[1].value()/100.0)*(120-1e-5) + 1e-5;
  double  x4 = (x[2].value()/100.0)*(5000-1e-5) + 1e-5;
  double  x5 = (x[3].value()/100.0)*(2000-1e-5) + 1e-5;
  double  x6 = (x[4].value()/100.0)*8 + 85;
  double  x7 = (x[5].value()/100.0)*5 + 90;
  double  x8 = (x[6].value()/100.0)*9 + 3;
  double  x9 = (x[7].value()/100.0)*2.8 + 1.2;
  double x10 = (x[8].value()/100.0)*17 + 145;

  double  x1 = 1.22*x4-x5;

  double a = 0.99 , b = 0.90;

  double g1 = 1e-5 - x1;
  double g2 = x1 - 2000;
  double g3 = 0.222 * x10 + b * x9 - 35.82;
  double g4 = -0.222 * x10 - x9/b + 35.82;
  double g5 = -3 * x7 + a * x10 + 133;
  double g6 = 3 * x7 - x10/a - 133;

  double f1 = 5.04*x1 + 0.035*x2 + 10*x3 +3.36*x5 - 0.063*x4*x7;
  double fi = f1 + 500 * (1.12*x1 + 0.13167*x1*x8 - 0.00667*x1*x8*x8 - x4/a);
  
  double z = (f1 > fi) ? f1 : fi;

  fi = f1 - 500 * (1.12*x1 + 0.13167*x1*x8 - 0.00667*x1*x8*x8 - a*x4);
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * (1.098*x8 - 0.038*x8*x8 + 0.325*x6 - x7/a + 57.425);
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * (1.098*x8 - 0.038*x8*x8 + 0.325*x6 - a*x7 + 57.425);
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * ( 98000*x3/(x4*x9+1000*x3) - x6 );
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * ( 98000*x3/(x4*x9+1000*x3) - x6 );
  if ( fi > z )
    z = fi;
  fi = f1 + 500 * ( (x2+x5)/x1 - x8 );
  if ( fi > z )
    z = fi;
  fi = f1 - 500 * ( (x2+x5)/x1 - x8 );
  if ( fi > z )
    z = fi;

  x.set_bb_output ( 0 , g1 );
  x.set_bb_output ( 1 , g2 );
  x.set_bb_output ( 2 , g3 );
  x.set_bb_output ( 3 , g4 );
  x.set_bb_output ( 4 , g5 );
  x.set_bb_output ( 5 , g6 );
  x.set_bb_output ( 6 , z  );

  count_eval = true;
  
  return true;
}
