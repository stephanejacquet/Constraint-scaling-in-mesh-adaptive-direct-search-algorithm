#include "More_Wild.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
More_Wild::More_Wild ( int n_instance , MW_pb_type pb_type )
  : Problem ( get_pb_id ( n_instance , pb_type )                  ,
	      "MORE_WILD"                                         ,
	      "xe_" + get_pb_id ( n_instance , pb_type ) + ".txt" ,
	      get_pb_n ( n_instance )                             ,
	      1                                                     ) {

  set_bbot ( 0, NOMAD::OBJ );

  std::string id;
  int         n;
  bool        ns;

  _MW_pb_type = pb_type;
  get_instance_data ( n_instance , pb_type , _MW_nprob , n , _MW_m , ns , id );

  // starting point:
  double * x0_tmp = new double [n];
  dfoxs ( n , _MW_nprob , ( ns ) ? 10.0 : 1.0 , x0_tmp );
  NOMAD::Point x0 ( n );
  for ( int i = 0 ; i < n ; ++i )
    x0[i] = x0_tmp[i];
  delete [] x0_tmp;
  set_x0 ( x0 );

  // lower bounds for some NONDIFF problems:
  if ( pb_type == MW_NONDIFF &&
       ( _MW_nprob  ==  8 ||
	 _MW_nprob  ==  9 ||
	 _MW_nprob  == 13 || 
	 _MW_nprob  == 16 ||
	 _MW_nprob  == 17 ||
	 _MW_nprob  == 18    ) )
    set_bounds ( NOMAD::Point ( n , 0.0 ) , NOMAD::Point() );

  // lower bound on the objective:
  set_f_lb ( 0.0 );

  if ( _MW_nprob == 1 ) {
    if ( pb_type == MW_SMOOTH )
      set_f_lb ( 36 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 22 );
    else if ( pb_type == MW_WILD3 )
      set_f_lb ( 35 );
  }
  else if ( _MW_nprob == 2 ) {
    if ( pb_type == MW_SMOOTH || pb_type == MW_WILD3 )
      set_f_lb ( 8 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 14 );
  }
  else if ( _MW_nprob == 3 ) {
    if ( pb_type == MW_SMOOTH )
      set_f_lb ( 9 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 15 );
    else if ( pb_type == MW_WILD3 )
      set_f_lb ( 9 );
  }
  else if ( _MW_nprob == 10 ) {
    if ( pb_type == MW_SMOOTH )
      set_f_lb ( 50000 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 1000 );
    else if ( pb_type == MW_WILD3 )
      set_f_lb ( 2e7 );
  }

  else if ( _MW_nprob == 13 ) {
    if ( pb_type == MW_SMOOTH || pb_type == MW_WILD3 )
      set_f_lb ( 100 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 30 );
  }

  else if ( _MW_nprob == 14 ) {
    if ( pb_type == MW_SMOOTH || pb_type == MW_WILD3 )
      set_f_lb ( 85000 );
    else if ( pb_type == MW_NONDIFF )
      set_f_lb ( 903 );
  }

  // keywords:
  add_keyword ( "published"       );
  add_keyword ( "more_wild_paper" );
  switch ( pb_type ) {
  case MW_SMOOTH : add_keyword ( "smooth"  ); break;
  case MW_NONDIFF: add_keyword ( "nondiff" ); break;
  case MW_WILD3  : add_keyword ( "wild3"   ); break;
  case MW_NOISY3 : add_keyword ( "noisy3"  ); break;
  }
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool More_Wild::eval_x ( NOMAD::Eval_Point & x          ,
			 bool              & count_eval   ) const {

  int    i , n = get_n();
  double tmp , f = 0.0 , n1 = 0.0 , n2 = 0.0 , ninf = 0.0 ,
       * fvec = new double[_MW_m] , * xc = new double[n];

  // restrict domain for some nondiff problems:
  if ( _MW_pb_type == MW_NONDIFF &&
       ( _MW_nprob  ==  8 ||
	 _MW_nprob  ==  9 ||
	 _MW_nprob  == 13 || 
	 _MW_nprob  == 16 ||
	 _MW_nprob  == 17 ||
	 _MW_nprob  == 18    ) )
    for ( i = 0 ; i < n ; ++i )
      xc[i] = ( x[i].value() < 0.0 ) ? 0.0 : x[i].value();
  else if ( _MW_pb_type == MW_WILD3 ) {
    for ( i = 0 ; i < n ; ++i ) {
      xc[i] = x[i].value();
      tmp = fabs ( x[i].value() );
      n1 += tmp;
      n2 += x[i].value()*x[i].value();
      if ( tmp > ninf )
	ninf = tmp;
    }
    n2 = sqrt(n2);
  }
  else
    for ( i = 0 ; i < n ; ++i )
      xc[i] = x[i].value();

  // generate the vector:
  if ( !dfovec ( _MW_m , n , xc , _MW_nprob , fvec ) ) {
    delete [] xc;
    delete [] fvec;
    count_eval = true;
    return false;
  }

  delete [] xc;

  // calculate the function value:
  switch ( _MW_pb_type ) {

  case MW_SMOOTH: // smooth
    for ( i = 0 ; i < _MW_m ; ++i )
      f += fvec[i]*fvec[i];
    break;
    
  case MW_NONDIFF: // nonsmooth
    for ( i = 0 ; i < _MW_m ; ++i )
      f += fabs ( fvec[i] );
    break;
    
  case MW_WILD3: // deterministic noise
    tmp  = 0.9*sin(100*n1)*cos(100*ninf) + 0.1*cos(n2);
    tmp *= 4.0*tmp*tmp - 3.0;
    for ( i = 0 ; i < _MW_m ; ++i )
      f += fvec[i]*fvec[i];
    f *= 1.0 + 0.001*tmp;
    break;

  case MW_NOISY3: // random noise
    for ( i = 0 ; i < _MW_m ; ++i ) {
      tmp  = rand() / MW_DINT_MAX;
      tmp  = 1.0 + 0.001 * ( tmp * 2.0 - 1.0 );
      tmp *= fvec[i];
      f += tmp*tmp;
    }    
    break;
  }

  delete [] fvec;

  x.set_bb_output ( 0 , f );

  count_eval = true;
  
  return true;
}


/*----------------------------------------------*/
/*              get problem dimension           */
/*----------------------------------------------*/
int More_Wild::get_pb_n ( int n_instance ) const {
  switch ( n_instance ) {
  case  1: return  9;
  case  2: return  9;
  case  3: return  7;
  case  4: return  7;
  case  5: return  7;
  case  6: return  7;
  case  7: return  2;
  case  8: return  2;
  case  9: return  3;
  case 10: return  3;
  case 11: return  4;
  case 12: return  4;
  case 13: return  2;
  case 14: return  2;
  case 15: return  3;
  case 16: return  3;
  case 17: return  4;
  case 18: return  3;
  case 19: return  6;
  case 20: return  6;
  case 21: return  9;
  case 22: return  9;
  case 23: return 12;
  case 24: return 12;
  case 25: return  3;
  case 26: return  2;
  case 27: return  4;
  case 28: return  4;
  case 29: return  6;
  case 30: return  7;
  case 31: return  8;
  case 32: return  9;
  case 33: return 10;
  case 34: return 11;
  case 35: return 10;
  case 36: return  5;
  case 37: return 11;
  case 38: return 11;
  case 39: return  8;
  case 40: return 10;
  case 41: return 11;
  case 42: return 12;
  case 43: return  5;
  case 44: return  6;
  case 45: return  8;
  case 46: return  5;
  case 47: return  5;
  case 48: return  8;
  case 49: return 10;
  case 50: return 12;
  case 51: return 12;
  case 52: return  8;
  case 53: return  8;
  }
  return 0;
}

/*----------------------*/
/*   get problem name   */
/*----------------------*/
void More_Wild::get_pb_name ( int           nprob       ,
			      std::string & name        ,
			      std::string & description   ) const {
  switch ( nprob ) {
  case  1:
    name        = "LINEAR_FULL";
    description = "Linear function - full rank";
    break;
  case  2:
    name        = "LINEAR_R1";
    description = "Linear function - rank 1";
    break;
  case  3:
    name        = "LINEAR_R1Z";
    description = "Linear function - rank 1 with zero columns and rows";
    break;
  case  4:
    name        = "ROSENBROCK";
    description = "Rosenbrock function";
    break;
  case  5:
    name        = "HELICAL";
    description = "Helical valley function";
    break;
  case  6:
    name        = "POWELLSG";
    description = "Powell singular function";
    break;
  case  7:
    name        = "FREUDENSTEIN_ROTH";
    description = "Freudenstein and Roth function";
    break;
  case  8:
    name        = "BARD";
    description = "Bard function";
    break;
  case  9:
    name        = "KOWALIK_OSBORNE";
    description = "Kowalik and Osborne function";
    break;
  case 10:
    name        = "MEYER";
    description = "Meyer function";
    break;
  case 11:
    name        = "WATSON";
    description = "Watson function";
    break;
  case 12:
    name        = "BOX3";
    description = "Box 3-dimensional function";
    break;
  case 13:
    name        = "JENNRICH_SAMPSON";
    description = "Jennrich and Sampson function";
    break;
  case 14:
    name        = "BROWN_DENNIS";
    description = "Brown and Dennis function";
    break;
  case 15:
    name        = "CHEBYQUAD";
    description = "Chebyquad function";
    break;
  case 16:
    name         = "BROWN";
    description = "Brown almost-linear function";
    break;
  case 17:
    name        = "OSBORNE1";
    description = "Osborne 1 function";
    break;
  case 18:
    name        = "OSBORNE2";
    description = "Osborne 2 function";
    break;
  case 19:
    name        = "BDQRTIC";
    description = "Bdqrtic function";
    break;
  case 20:
    name        = "CUBE";
    description = "Cube function";
    break;
  case 21:
    name        = "MANCINO";
    description = "Mancino function";
    break;
  case 22:
    name        = "HEART8";
    description = "Heart8 function";
    break;
  }
}

/*----------------------------------------------*/
/*                 get problem id               */
/*----------------------------------------------*/
std::string More_Wild::get_pb_id ( int n_instance , MW_pb_type pb_type ) const {

  std::string id;
  int         nprob , n , m;
  bool        ns;

  get_instance_data ( n_instance , pb_type , nprob , n , m , ns , id );

  return id;
}

/*-----------------------*/
/*   get instance data   */
/*-----------------------*/
void More_Wild::get_instance_data ( int               instance ,
				    MW_pb_type        pbtype   ,
				    int             & nprob    ,
				    int             & n        ,
				    int             & m        ,
				    bool            & ns       ,
				    std::string     & name       ) const {
  switch ( instance ) {
  case  1: nprob =  1; n =  9; m = 45; ns = 0; break; // LINEAR_FULL
  case  2: nprob =  1; n =  9; m = 45; ns = 1; break; // LINEAR_FULL
  case  3: nprob =  2; n =  7; m = 35; ns = 0; break; // LINEAR_R1
  case  4: nprob =  2; n =  7; m = 35; ns = 1; break; // LINEAR_R1
  case  5: nprob =  3; n =  7; m = 35; ns = 0; break; // LINEAR_R1Z
  case  6: nprob =  3; n =  7; m = 35; ns = 1; break; // LINEAR_R1Z
  case  7: nprob =  4; n =  2; m =  2; ns = 0; break; // ROSENBROCK
  case  8: nprob =  4; n =  2; m =  2; ns = 1; break; // ROSENBROCK
  case  9: nprob =  5; n =  3; m =  3; ns = 0; break; // HELICAL
  case 10: nprob =  5; n =  3; m =  3; ns = 1; break; // HELICAL
  case 11: nprob =  6; n =  4; m =  4; ns = 0; break; // POWELLSG
  case 12: nprob =  6; n =  4; m =  4; ns = 1; break; // POWELLSG
  case 13: nprob =  7; n =  2; m =  2; ns = 0; break; // FREUDENSTEIN_ROTH
  case 14: nprob =  7; n =  2; m =  2; ns = 1; break; // FREUDENSTEIN_ROTH
  case 15: nprob =  8; n =  3; m = 15; ns = 0; break; // BARD
  case 16: nprob =  8; n =  3; m = 15; ns = 1; break; // BARD
  case 17: nprob =  9; n =  4; m = 11; ns = 0; break; // KOWALIK_OSBORNE
  case 18: nprob = 10; n =  3; m = 16; ns = 0; break; // MEYER
  case 19: nprob = 11; n =  6; m = 31; ns = 0; break; // WATSON
  case 20: nprob = 11; n =  6; m = 31; ns = 1; break; // WATSON
  case 21: nprob = 11; n =  9; m = 31; ns = 0; break; // WATSON
  case 22: nprob = 11; n =  9; m = 31; ns = 1; break; // WATSON
  case 23: nprob = 11; n = 12; m = 31; ns = 0; break; // WATSON
  case 24: nprob = 11; n = 12; m = 31; ns = 1; break; // WATSON
  case 25: nprob = 12; n =  3; m = 10; ns = 0; break; // BOX3
  case 26: nprob = 13; n =  2; m = 10; ns = 0; break; // JENNRICH_SAMPSON
  case 27: nprob = 14; n =  4; m = 20; ns = 0; break; // BROWN_DENNIS
  case 28: nprob = 14; n =  4; m = 20; ns = 1; break; // BROWN_DENNIS
  case 29: nprob = 15; n =  6; m =  6; ns = 0; break; // CHEBYQUAD
  case 30: nprob = 15; n =  7; m =  7; ns = 0; break; // CHEBYQUAD
  case 31: nprob = 15; n =  8; m =  8; ns = 0; break; // CHEBYQUAD
  case 32: nprob = 15; n =  9; m =  9; ns = 0; break; // CHEBYQUAD
  case 33: nprob = 15; n = 10; m = 10; ns = 0; break; // CHEBYQUAD
  case 34: nprob = 15; n = 11; m = 11; ns = 0; break; // CHEBYQUAD
  case 35: nprob = 16; n = 10; m = 10; ns = 0; break; // BROWN
  case 36: nprob = 17; n =  5; m = 33; ns = 0; break; // OSBORNE1
  case 37: nprob = 18; n = 11; m = 65; ns = 0; break; // OSBORNE2
  case 38: nprob = 18; n = 11; m = 65; ns = 1; break; // OSBORNE2
  case 39: nprob = 19; n =  8; m =  8; ns = 0; break; // BDQRTIC
  case 40: nprob = 19; n = 10; m = 12; ns = 0; break; // BDQRTIC
  case 41: nprob = 19; n = 11; m = 14; ns = 0; break; // BDQRTIC
  case 42: nprob = 19; n = 12; m = 16; ns = 0; break; // BDQRTIC
  case 43: nprob = 20; n =  5; m =  5; ns = 0; break; // CUBE
  case 44: nprob = 20; n =  6; m =  6; ns = 0; break; // CUBE
  case 45: nprob = 20; n =  8; m =  8; ns = 0; break; // CUBE
  case 46: nprob = 21; n =  5; m =  5; ns = 0; break; // MANCINO
  case 47: nprob = 21; n =  5; m =  5; ns = 1; break; // MANCINO
  case 48: nprob = 21; n =  8; m =  8; ns = 0; break; // MANCINO
  case 49: nprob = 21; n = 10; m = 10; ns = 0; break; // MANCINO
  case 50: nprob = 21; n = 12; m = 12; ns = 0; break; // MANCINO
  case 51: nprob = 21; n = 12; m = 12; ns = 1; break; // MANCINO
  case 52: nprob = 22; n =  8; m =  8; ns = 0; break; // HEART8
  case 53: nprob = 22; n =  8; m =  8; ns = 1; break; // HEART8
  }

  std::string pb_name , tmp;
  get_pb_name ( nprob , pb_name , tmp );

  std::ostringstream oss;
  oss << "MORE_WILD_" << instance << "_" << pb_name;

  switch ( pbtype ) {
  case MW_SMOOTH : oss << "_SMOOTH" ; break;
  case MW_NONDIFF: oss << "_NONDIFF"; break;
  case MW_WILD3  : oss << "_WILD3"; break;
  case MW_NOISY3 : oss << "_NOISY3"; break;
  }

  name = oss.str();
}

/*--------------------------------------------------*/
/*                   dfoxs function                 */
/*--------------------------------------------------*/
/* transcripted in C++ from the matlab version      */
/* dfosx.m of S. Wild                               */
/* http://www.mcs.anl.gov/~more/dfo/matlab/dfoxs.m  */
/*--------------------------------------------------*/
void More_Wild::dfoxs ( int      n      ,
			int      nprob  ,
			double   factor ,
			double * x        ) const {

  int    i , j;
  double sum , tmp;

  switch ( nprob ) {
  case  1: // Linear function - full rank or rank 1
  case  2: // Linear function - full rank or rank 1
  case  3: // Linear function - full rank or rank 1
  case  8: // Bard function
  case 19: // Bdqrtic
    for ( i = 0 ; i < n ; ++i )
      x[i] = 1.0;
    break;
  case 4: // Rosenbrock function
    x[0] = -1.2;
    x[1] =  1.0;
    break;
  case 5: // Helical valley function
    x[0] = -1;
    for ( i = 1 ; i < n ; ++i )
      x[i] = 0.0;
    break;
  case 6: // Powell singular function
    x[0] =  3;
    x[1] = -1;
    x[2] =  0;
    x[3] =  1;
    break;
  case 7: // Freudenstein and Roth function
    x[0] =  0.5;
    x[1] = -2.0;
    break;
  case 9: // Kowalik and Osborne function
    x[0] = 0.250;
    x[1] = 0.390;
    x[2] = 0.415;
    x[3] = 0.390;
    break;
  case 10: // Meyer function
    x[0] = .02;
    x[1] = 4000;
    x[2] = 250;
    break;
  case 11: // Watson function
  case 16: // Brown almost-linear function
  case 20: // Cube
    for ( i = 0 ; i < n ; ++i )
      x[i] = 0.5;
    break;
  case 12: // Box 3-dimensional function
    x[0] = 0;
    x[1] = 10;
    x[2] = 20;
    break;
  case 13: // Jennrich and Sampson function
    x[0] = 0.3;
    x[1] = 0.4;
    break;
  case 14: // Brown and Dennis function
    x[0] = 25;
    x[1] =  5;
    x[2] = -5;
    x[3] = -1;
    break;
  case 15: // Chebyquad function
    for ( i = 1 ; i <= n ; ++i )
      x[i-1] = i/(n+1.0);
    break;
  case 17: // Osborne 1 function
    x[0] = 0.5;
    x[1] = 1.5;
    x[2] = 1.0;
    x[3] = 0.01;
    x[4] = 0.02;
    break;
  case 18: // Osborne 2 function
    x[ 0] = 1.3;
    x[ 1] = 0.65;
    x[ 2] = 0.65;
    x[ 3] = 0.7;
    x[ 4] = 0.6;
    x[ 5] = 3.0;
    x[ 6] = 5.0;
    x[ 7] = 7.0;
    x[ 8] = 2.0;
    x[ 9] = 4.5;
    x[10] = 5.5;
    break;
  case 21: // Mancino
    for ( i = 1 ; i <= n ; ++i ) {
      sum = 0;
      for ( j = 1 ; j <= n ; ++j ) {
	tmp  = i;
	tmp  = sqrt(tmp/j);
	sum += (tmp*( pow(sin(log(tmp)),5.0)+ pow(cos(log(tmp)),5.0)));
      }
      x[i-1] = -0.0008710996 * (pow(i-50,3.0) + sum);
    }
    break;
  case 22: // Heart8
    x[0] = -0.300;
    x[1] = -0.390;
    x[2] =  0.300;
    x[3] = -0.344;
    x[4] = -1.200;
    x[5] =  2.690;
    x[6] =  1.590;
    x[7] = -1.500;
    break;
  }

  for ( i = 0 ; i < n ; ++i )
    x[i] *= factor;
}

/*---------------------------------------------------*/
/*                  dfovec function                  */
/*---------------------------------------------------*/
/* transcripted in C++ from the matlab version       */
/* dfovec.m of S. Wild                               */
/* http://www.mcs.anl.gov/~more/dfo/matlab/dfovec.m  */
/*---------------------------------------------------*/
bool More_Wild::dfovec ( int            m     ,    // IN : number of outputs
			 int            n     ,    // IN : dimension of problem
			 const double * x     ,    // IN : array of size n
			 int            nprob ,    // IN : problem index in {1,2,...,22}
			 double       * fvec    ) const { // OUT: array of size m

  int    i , j;
  double tmp , tmp1 , tmp2 , tmp3 , tmp4 , den , sum = 0;

  // 1. Linear function - full rank:
  if ( nprob == 1 ) {
    for ( j = 0 ; j < n ; ++j )
      sum += x[j];
    tmp = 2*sum/m + 1;   
    for ( i = 0 ; i < m ; ++i ) {
      fvec[i] = -tmp;
      if ( i < n )
	fvec[i] += x[i];
    }
  }

  // 2. Linear function - rank 1:
  else if ( nprob == 2 ) {
    for ( j = 1 ; j <= n ; ++j )
      sum += j*x[j-1];
    for ( i = 1 ; i <= m ; ++i )
      fvec[i-1] = i*sum - 1;
  }

  // 3. Linear function - rank 1 with zero columns and rows:
  else if ( nprob == 3 ) {
    for ( j = 2 ; j < n ; ++j ) 
      sum += j*x[j-1];
    for ( i = 0 ; i < m-1 ; ++i )
      fvec[i] = i*sum - 1;
    fvec[m-1] = -1;
  }

  // 4. Rosenbrock function:
  else if ( nprob == 4 ) {
    fvec[0] = 10*(x[1] - x[0]*x[0]);
    fvec[1] = 1 - x[0];
  }

  // 5. Helical valley function:
  else if ( nprob == 5 ) {
    if ( x[0] > 0 )
      tmp = atan ( x[1]/x[0] ) / (2*MW_PI);
    else if ( x[0] < 0 )
      tmp = atan ( x[1]/x[0] ) / (2*MW_PI) + .5;
    else
      tmp = .25;
    fvec[0] = 10*(x[2] - 10*tmp);
    fvec[1] = 10*(sqrt(x[0]*x[0]+x[1]*x[1])-1);
    fvec[2] = x[2];
  }

  // 6. Powell singular function:
  else if ( nprob == 6 ) {
    fvec[0] = x[0] + 10*x[1];
    fvec[1] = MW_SQRT5*(x[2] - x[3]);
    fvec[2] = pow(x[1] - 2*x[2],2.0);
    fvec[3] = MW_SQRT10*pow(x[0] - x[3],2.0);
  }

  // 7. Freudenstein and Roth function:
  else if ( nprob == 7 ) {
    fvec[0] = -MW_C13 + x[0] + ((5 - x[1])*x[1] - 2)*x[1];
    fvec[1] = -MW_C29 + x[0] + ((1 + x[1])*x[1] - MW_C14)*x[1];
  }

  // 8. Bard function:
  else if ( nprob == 8 ) {
    for ( i = 1 ; i <= 15 ; ++i ) {
      tmp1 = i;
      tmp2 = 16-i;
      tmp3 = tmp1;
      if ( i > 8 )
	tmp3 = tmp2;

      den = x[1]*tmp2 + x[2]*tmp3;
      
      if ( den == 0 )
	return false;

      fvec[i-1] = MW_Y1[i-1] - (x[0] + tmp1/den);
    }
  }

  // 9. Kowalik and Osborne function:
  else if ( nprob == 9 ) {
    for ( i = 0 ; i < 11 ; ++i ) {
      tmp1 = MW_V[i]*(MW_V[i] + x[1]);
      tmp2 = MW_V[i]*(MW_V[i] + x[2]) + x[3];
      if ( tmp2 == 0.0 )
	return false;
      fvec[i] = MW_Y2[i] - x[0]*tmp1/tmp2;
    }
  }

  // 10. Meyer function:
  else if ( nprob == 10 ) {
    for ( i = 0 ; i < 16 ; ++i ) {
      den = 5*(i+1) + MW_C45 + x[2];
       if ( den == 0 )
	return false;
      fvec[i] = x[0]*exp(x[1]/den) - MW_Y3[i];
    }
  }

  // 11. Watson function:
  else if ( nprob == 11 ) {
    for ( i = 1 ; i <= 29 ; ++i ) {
      tmp = i/MW_C29;
      tmp1 = 0;
      tmp3 = 1;
      for ( j = 2 ; j <= n ; ++j ) {
	tmp1 += (j-1)*tmp3*x[j-1];
	tmp3 *= tmp;
      }
      tmp2 = 0;
      tmp3 = 1;
      for ( j = 1 ; j <= n ; ++j ) {
	tmp2 += tmp3*x[j-1];
	tmp3 *= tmp;
      }
      fvec[i-1] = tmp1 - tmp2*tmp2 - 1;
    }
    fvec[29] = x[0];
    fvec[30] = x[1] - x[0]*x[0] - 1;
  }

  // 12. Box 3-dimensional function:
  else if ( nprob == 12 ) {
    for ( i = 1 ; i <= m ; ++i ) {
      tmp = i;
      tmp1 = tmp/10.0;
      fvec[i-1] = exp(-tmp1*x[0]) - exp(-tmp1*x[1]) +
	          (exp(-tmp) - exp(-tmp1))*x[2];
    }
  }

  // 13. Jennrich and Sampson function:
  else if ( nprob == 13 ) {
    for ( i = 1 ; i <= m ; ++i )
      fvec[i-1] = 2 + 2*i - exp(i*x[0]) - exp(i*x[1]);
  }

  // 14. Brown and Dennis function:
  else if ( nprob == 14 ) {
    for ( i = 1 ; i <= m ; ++i ) {
      tmp = i/5.0;
      tmp1 = x[0] + tmp*x[1] - exp(tmp);
      tmp2 = x[2] + sin(tmp)*x[3] - cos(tmp);
      fvec[i-1] = tmp1*tmp1 + tmp2*tmp2;
    }
  }

  // 15. Chebyquad function:
  else if ( nprob == 15 ) {
    for ( i = 0 ; i < m ; ++i )
      fvec[i] = 0.0;
    for ( j = 0 ; j < n ; ++j ) {
      tmp1 = 1;
      tmp2 = 2*x[j] - 1;
      tmp3 = 2*tmp2;
      for ( i = 0 ; i < m ; ++i ) {
	fvec[i] += tmp2;
	tmp = tmp3*tmp2 - tmp1;
	tmp1 = tmp2;
	tmp2 = tmp;
      }
    }
    tmp = -1;
    for ( i = 1 ; i <= m ; ++i ) {
      fvec[i-1] /= n;
      if ( tmp > 0 )
	fvec[i-1] += 1.0 / (i*i - 1);
      tmp = -tmp;
    }
  }

  // 16. Brown almost-linear function:
  else if ( nprob == 16 ) {
    sum = -n-1;
    tmp = 1;
    for ( j = 0 ; j < n ; ++j ) {
      sum += x[j];
      tmp *= x[j];
    }
    for ( i = 0 ; i < n-1 ; ++i )
      fvec[i] = x[i] + sum;
    fvec[n-1] = tmp - 1;
  }

  // 17. Osborne 1 function:
  else if ( nprob == 17 ) {
    for ( i = 1 ; i <= 33 ; ++i ) {
      tmp = 10*(i-1);
      tmp1 = exp(-x[3]*tmp);
      tmp2 = exp(-x[4]*tmp);
      fvec[i-1] = MW_Y4[i-1] - (x[0] + x[1]*tmp1 + x[2]*tmp2);
    }
  }

  // 18. Osborne 2 function:
  else if ( nprob == 18 ) {
    for ( i = 0 ; i < 65 ; ++i ) {
      tmp = i/10.0;
      tmp1 = exp(-x[4]*tmp);
      tmp2 = exp(-x[5]*pow(tmp-x[8],2.0));
      tmp3 = exp(-x[6]*pow(tmp-x[9],2.0));
      tmp4 = exp(-x[7]*pow(tmp-x[10],2.0));
      fvec[i] = MW_Y5[i] - (x[0]*tmp1 + x[1]*tmp2 +
			    x[2]*tmp3 + x[3]*tmp4);
    }
  }

  // 19. Bdqrtic:
  else if ( nprob == 19 ) {
    for ( i = 1 ; i <= n-4 ; ++i ) {
      fvec[i-1  ] = (-4*x[i-1]+3.0);
      fvec[n-5+i] = (x[i-1]*x[i-1]+2*x[i]*x[i]+3*x[i+1]*x[i+1]
		     +4*x[i+2]*x[i+2]+5*x[n-1]*x[n-1]);
    }
  }

  // 20. Cube:
  else if ( nprob == 20 ) {
    fvec[0] = (x[0]-1.0);
    for ( i = 1 ; i < n ; ++i )
      fvec[i] = 10*(x[i]-pow(x[i-1],3.0));
  }

  // 21. Mancino:
  else if ( nprob == 21 ) {
    for ( i = 1 ; i <= n ; ++i ) {
      tmp1 = 0;
      for ( j = 1 ; j <= n ; ++j ) {
	tmp3 = i;
	tmp2 = sqrt ( x[i-1]*x[i-1] + tmp3/j) ;
	tmp1 += tmp2*( pow(sin(log(tmp2)),5.0) + pow(cos(log(tmp2)),5.0) );
      }
      fvec[i-1]=1400*x[i-1] + pow(i-50,3.0) + tmp1;
    }
  }

  // 22. Heart8:
  else if ( nprob == 22 ) {
    fvec[0] = x[0] + x[1] + 0.69;
    fvec[1] = x[2] + x[3] + 0.044;
    fvec[2] = x[4]*x[0] + x[5]*x[1] - x[6]*x[2] - x[7]*x[3] + 1.57;
    fvec[3] = x[6]*x[0] + x[7]*x[1] + x[4]*x[2] + x[5]*x[3] + 1.31;
    fvec[4] = x[0]*(x[4]*x[4]-x[6]*x[6]) - 2.0*x[2]*x[4]*x[6] +
      x[1]*(x[5]*x[5]-x[7]*x[7]) - 2.0*x[3]*x[5]*x[7] + 2.65;
    fvec[5] = x[2]*(x[4]*x[4]-x[6]*x[6]) + 2.0*x[0]*x[4]*x[6] +
      x[3]*(x[5]*x[5]-x[7]*x[7]) + 2.0*x[1]*x[5]*x[7] - 2.0;
    fvec[6] = x[0]*x[4]*(x[4]*x[4]-3.0*x[6]*x[6]) +
      x[2]*x[6]*(x[6]*x[6]-3.0*x[4]*x[4]) +
      x[1]*x[5]*(x[5]*x[5]-3.0*x[7]*x[7]) +
      x[3]*x[7]*(x[7]*x[7]-3.0*x[5]*x[5]) + 12.6;
    fvec[7] = x[2]*x[4]*(x[4]*x[4]-3.0*x[6]*x[6]) -
      x[0]*x[6]*(x[6]*x[6]-3.0*x[4]*x[4]) +
      x[3]*x[5]*(x[5]*x[5]-3.0*x[7]*x[7]) -
      x[1]*x[7]*(x[7]*x[7]-3.0*x[5]*x[5]) - 9.48;
  }

  return true;
}
