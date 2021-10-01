#include "OptEng_RBF.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
OptEng_RBF::OptEng_RBF ( void )
  : Problem ( "OPTENG_RBF" , "OPTENG_RBF" , "xe.txt" , 3 , 5 ) {

  set_bbot ( 0 , NOMAD::OBJ );
  set_bbot ( 1 , NOMAD::PB  );
  set_bbot ( 2 , NOMAD::PB  );
  set_bbot ( 3 , NOMAD::PB  );
  set_bbot ( 4 , NOMAD::PB  );

  NOMAD::Point x0 ( 3 , 0.0 );
  x0[0] = 1.025;
  x0[1] = 0.775;
  x0[2] = 8.5;
  set_x0 ( x0 );

  set_f_lb ( 0.0 );

  NOMAD::Point lb ( 3 , 0.0 );
  NOMAD::Point ub ( 3 , 0.0 );

  lb[0] = 0.05;
  lb[1] = 0.25;
  lb[2] = 2.0;

  ub[0] =  2.0;
  ub[1] =  1.3;
  ub[2] = 15.0;

  set_bounds ( lb , ub );

  add_keyword ( "published" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool OptEng_RBF::eval_x ( NOMAD::Eval_Point & x          ,
			  bool              & count_eval   ) const {
  double x1 = x[0].value();
  double x2 = x[1].value();
  double x3 = x[2].value();

  double f = (2+x3)*x1*x1*x2;

  double g1 = 1 - pow(x2,3.0)*x3/(71785*pow(x1,4.0));
  double g2 = (4*x2*x2-x1*x2)/(12566*(x2*pow(x1,3.0)-pow(x1,4.0)))
              + 1/(5108*x1*x1) - 1;
  double g3 = 1 - 140.45*x1/(x2*x2*x3);
  double g4 = (x1+x2)/1.5 - 1;

  x.set_bb_output ( 0 , f );
  x.set_bb_output ( 1 , g1 );
  x.set_bb_output ( 2 , g2 );
  x.set_bb_output ( 3 , g3 );
  x.set_bb_output ( 4 , g4 );

  count_eval = true;
  
  return true;
}
