#include "Osborne2.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Osborne2::Osborne2 ( void )
  : Problem ( "OSBORNE2" , "OSBORNE2" , "xe.txt" , 11 , 1 ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( 11 );

  x0[ 0] = 1.30;
  x0[ 1] = 0.65;
  x0[ 2] = 0.65;
  x0[ 3] = 0.70;
  x0[ 4] = 0.60;
  x0[ 5] = 3.00;
  x0[ 6] = 5.00;
  x0[ 7] = 7.00;
  x0[ 8] = 2.00;
  x0[ 9] = 4.50;
  x0[10] = 5.50;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
  add_keyword ( "mads_dfo_paper"  );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Osborne2::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  double y[65] = {
    1.366 , 1.191 , 1.112 , 1.013 , 0.991 , 0.885 , 0.831 ,
    0.847 , 0.786 , 0.725 , 0.746 , 0.679 , 0.608 , 0.655 ,
    0.616 , 0.606 , 0.602 , 0.626 , 0.651 , 0.724 , 0.649 ,
    0.649 ,
    0.694 , 0.644 , 0.624 , 0.661 , 0.612 , 0.558 , 0.533 ,
    0.495 , 0.500 , 0.423 , 0.395 , 0.375 , 0.372 , 0.391 ,
    0.396 , 0.405 , 0.428 , 0.429 , 0.523 , 0.562 , 0.607 ,
    0.653 ,
    0.672 , 0.708 , 0.633 , 0.668 , 0.645 , 0.632 , 0.591 ,
    0.559 , 0.597 , 0.625 , 0.739 , 0.710 , 0.729 , 0.720 ,
    0.636 , 0.581 , 0.428 , 0.292 , 0.162 , 0.098 , 0.054   };

  count_eval = true;
  
  double ti , fi;
  double z = -1e+20;

  for ( int i = 1 ; i <= 65 ; i++ ) {
    ti = 0.1*(i-1);
    fi = y[i-1]  -
      x[0].value()*exp(-x[4].value()*ti) -
      x[1].value()*exp(-x[5].value()*(ti-x[8].value())*(ti-x[8].value())) -
      x[2].value()*exp(-x[6].value()*(ti-x[9].value())*(ti-x[9].value())) -
      x[3].value()*exp(-x[7].value()*(ti-x[10].value())*(ti-x[10].value()));

    if ( fabs(fi) > z )
      z = fabs(fi);
    
    if ( z > 1e+20 || z < 0.0 ) {
      x.set_bb_output ( 0 , 1e+20 );
      return true;
    }
    
  }

  x.set_bb_output ( 0 , z );

  return true;
}
