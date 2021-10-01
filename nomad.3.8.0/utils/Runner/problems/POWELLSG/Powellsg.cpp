#include "Powellsg.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Powellsg::Powellsg ( int n )
  : Problem ( "POWELLSG" + NOMAD::itos(n)          ,
	      "POWELLSG"                           ,
	      "xe_"      + NOMAD::itos(n) + ".txt" ,
	      n                                    ,
	      1                                      ) {

  set_bbot ( 0, NOMAD::OBJ );

  NOMAD::Point x0 ( n , 0.0 );

  int i , i4 , n4 = n/4;
  for ( i = 0 ; i < n4 ; ++i ) {
    i4 = 4*i;
    x0[i4  ] =  3;
    x0[i4+1] = -1;
    x0[i4+3] =  1;
  } 

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "smooth"    );
  add_keyword ( "published" );

  if ( n == 12 || n == 20 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Powellsg::eval_x ( NOMAD::Eval_Point & x          ,
			bool              & count_eval   ) const {

  int    i , i4 , n4 = get_n() / 4;
  double f     = 0.0;
  double r5    = sqrt(5.0);
  double r10   = sqrt(10.0);
  
  for ( i = 1 ; i <= n4 ; ++i ) {
    i4 = 4*i;
    f += pow ( x[i4-4].value() + 10* x[i4-3].value() , 2 );
    f += pow ( r5 * (x[i4-2].value() - x[i4-1].value()) , 2 );
    f += pow ( pow(x[i4-3].value()-2*x[i4-2].value(),2) , 2 );
    f += pow ( r10*pow(x[i4-4].value()-x[i4-1].value(),2) , 2 );
  }

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}
