#include "B250.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
B250::B250 ( void )
  : Problem ( "B250" , "B250" , "xe.txt" , 60 , 2 ) {

  set_bbot ( 0 , NOMAD::OBJ );
  set_bbot ( 1 , NOMAD::PB  );

  set_bounds ( NOMAD::Point ( 60 , 0.0 ) , NOMAD::Point ( 60 , 100.0 ) );

  NOMAD::Point x0 ( 60 );
  x0[ 0] = 50;
  x0[ 1] = 90;
  x0[ 2] = 50;
  x0[ 3] = 90;
  x0[ 4] = 50;
  x0[ 5] = 90;
  x0[ 6] = 50;
  x0[ 7] = 10;
  x0[ 8] = 90;
  x0[ 9] = 50;
  x0[10] = 90;
  x0[11] = 90;
  x0[12] = 10;
  x0[13] = 10;
  x0[14] = 50;
  x0[15] = 90;
  x0[16] = 50;
  x0[17] = 90;
  x0[18] = 90;
  x0[19] = 50;
  x0[20] = 90;
  x0[21] = 90;
  x0[22] = 90;
  x0[23] = 10;
  x0[24] = 10;
  x0[25] = 90;
  x0[26] = 50;
  x0[27] = 90;
  x0[28] = 50;
  x0[29] = 50;
  x0[30] = 50;
  x0[31] = 50;
  x0[32] = 50;
  x0[33] = 50;
  x0[34] = 10;
  x0[35] = 50;
  x0[36] = 90;
  x0[37] = 50;
  x0[38] = 10;
  x0[39] = 90;
  x0[40] = 10;
  x0[41] = 90;
  x0[42] = 90;
  x0[43] = 50;
  x0[44] = 90;
  x0[45] = 50;
  x0[46] = 90;
  x0[47] = 50;
  x0[48] = 10;
  x0[49] = 50;
  x0[50] = 50;
  x0[51] = 50;
  x0[52] = 10;
  x0[53] = 50;
  x0[54] = 50;
  x0[55] = 50;
  x0[56] = 50;
  x0[57] = 10;
  x0[58] = 50;
  x0[59] = 90;

  set_x0   ( x0  );
  set_f_lb ( 0.0 );

  add_keyword ( "published"       );
  add_keyword ( "orthomads_paper" );
}

/*---------------------------------------*/
/*           penalty function II         */
/*---------------------------------------*/
/*  S1 variables : 1 --> 15              */
/*    (n=15 variables)                   */
/*---------------------------------------*/
double B250::f_penalty ( double x[60] ) const {

  double f = pow ( x[0] - 0.2 , 2 );
  int    i;

  for ( i = 2 ; i <= 15 ; i++ )
    f += pow ( B250_SQRT_A * ( exp ( x[i-1] / 10 ) + exp ( x[i-2] / 10 )
			  - exp ( i/10.0 ) - exp ( (i-1)/10.0 ) ) , 2 );

  for ( i = 16 ; i < 30 ; i++ )
    f += pow ( B250_SQRT_A * ( exp ( x[i-15] / 10 ) - B250_E1 ) , 2 );

  double s = -1.0;
  for ( i = 1 ; i <= 15 ; i++ )
    s += (16-i) * pow ( x[i-1] , 2 );

  return f + pow(s,2);
}

/*---------------------------------------*/
/*             trigo. function           */
/*---------------------------------------*/
/*  S2 variables : 16 --> 45             */
/*    (n=30 variables)                   */
/*---------------------------------------*/
double B250::f_trigo ( double x[60] ) const {

  int    i;
  double s = cos(x[15]);
  for ( i = 16 ; i < 45 ; i++ )
    s += cos(x[i]);
  
  double f = 0.0;
  for ( i = 1 ; i <= 30 ; i++ )
    f += pow ( 30 - s + i * ( 1 - cos(x[i+14]) ) - sin (x[i+14]) , 2 );

  return f;
}

/*---------------------------------------*/
/*       Brown almost-linear function    */
/*---------------------------------------*/
/*  S3 variables : 46 --> 60             */
/*    (n=15 variables)                   */
/*---------------------------------------*/
double B250::f_brown ( double x[60] ) const {

  int i;

  double s = x[45];
  for ( i = 46 ; i < 60 ; i++ )
    s += x[i];

  double f = 0.0;
  for ( i = 1 ; i < 15 ; i++ )
    f += pow ( x[i+44] + s - 16 , 2 );

  s = x[45];
  for ( i = 46 ; i < 60 ; i++ )
    s *= x[i];
  
  return f + pow ( s-1 , 2 );
}

/*---------------------------------------*/
/*        Broyden banded function        */
/*---------------------------------------*/
/*  S1 variables : 1 --> 15              */
/*    (n=15 variables)                   */
/*---------------------------------------*/
double B250::f_broyden_banden ( double x[60] ) const {
  int i , j;
  double s , f = 0.0;
  for ( i = 1 ; i <= 15 ; i++ ) {
    s = 0.0;
    for ( j = 0 ; j < B250_L[i-1] ; j++ )
      s += x[ B250_J[i-1][j] - 1 ] * ( 1 + x[ B250_J[i-1][j] - 1 ] );
    f += pow ( x[i-1] * ( 2 + 5 * pow ( x[i-1] , 2 ) ) + 1 - s , 2 );
  }
  return f;
}

/*---------------------------------------*/
/*        Broyden tridiagonal function   */
/*---------------------------------------*/
/*  S2 variables : 16 --> 45             */
/*    (n=30 variables)                   */
/*---------------------------------------*/
double B250::f_broyden_tridiag ( double x[60] ) const {
  double f = pow ( ( 3 - 2 * x[15] ) * x[15] - 2 * x[16] + 1 , 2 );
  for ( int i = 16 ; i <= 43 ; i++ )
    f += pow ( ( 3 - 2 * x[i] ) * x[i] - x[i-1] - 2 * x[i+1] + 1 , 2 );
  return f + pow ( (3-2*x[44]) * x[44] - x[43] + 1 , 2 );
}

/*---------------------------------------*/
/*    discrete boundary value function   */
/*---------------------------------------*/
/*  S3 variables : 46 --> 60             */
/*    (n=15 variables)                   */
/*---------------------------------------*/
double B250::f_discrete_boundary ( double x[60] ) const {
  double f = pow ( 2*x[45] - x[46] + B250_H2
		   * pow( x[45] + B250_H + 1 , 3 ) / 2.0 , 2 );
  for ( int i = 2 ; i <= 14 ; i++ )
    f += pow ( 2*x[i+44] - x[i+43] - x[i+45] + B250_H2
	       * pow( x[i+44] + i * B250_H + 1 , 3 ) / 2.0 , 2 );
  return f + pow ( 2*x[59] - x[58] + B250_H2 *
		   pow( x[59] + 15 * B250_H + 1 , 3 ) / 2.0 , 2 );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool B250::eval_x ( NOMAD::Eval_Point & xx           ,
		    bool              & count_eval   ) const {
  double x[60];

  for ( int i = 0 ; i < 60 ; i++ ) {
   
    x[i] = xx[i].value();

    // scaling :
    // ---------
    
    // i in [0;14] :
    // [0;100] --> [-0.5;1.0] :
    if ( i <= 14 )
      x[i] = x[i]*3.0/200.0-0.5;

    // i in [15;44] :
    // [0;100] --> [-0.75;1.0] :
    else if ( i <= 44 )
      x[i] = x[i]*7.0/400.0-0.75;

    // i in [45;59] :
    // [0;100] --> [-0.2;2.0] :
    else
      x[i] = x[i]*11.0/500.0-0.2;
  }

  xx.set_bb_output ( 0 , f_penalty(x)+f_trigo(x)+f_brown(x) );
  xx.set_bb_output ( 1 , -f_broyden_banden(x)-f_broyden_tridiag(x)
		     -f_discrete_boundary(x) + 250 );
  
  count_eval = true;
  
  return true;
}
