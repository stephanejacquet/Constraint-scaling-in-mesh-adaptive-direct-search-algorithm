#include "Disk.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Disk::Disk ( int n )
  : Problem ( "DISK" + NOMAD::itos(n)          ,
	      "DISK"                           ,
	      "xe_"     + NOMAD::itos(n) + ".txt" ,
	      n                                   ,
	      2                                     ) {

  set_bbot ( 0, NOMAD::PB              );
  set_bbot ( 1, NOMAD::OBJ             );
  set_x0   ( NOMAD::Point ( n , 0.0 )  );
  
  if ( n == 10 )
    set_f_lb ( -20.0 );

  add_keyword ( "published" );
  add_keyword ( "quadratic" );

  if ( n == 10 ) {
    add_keyword ( "orthomads_paper" );
    add_keyword ( "mads_dfo_paper"  );
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Disk::eval_x ( NOMAD::Eval_Point & x          ,
		    bool              & count_eval   ) const {
  
  int    n = get_n();
  double z = 0.0 , h = 0.0;

  for ( int i = 0 ; i < n ; i++ ) {
    z += x[i].value();
    h += x[i].value()*x[i].value();
  }

  h -= 30.0;

  x.set_bb_output ( 0 , h );
  x.set_bb_output ( 1 , z );

  count_eval = true;
  
  return true;
}
