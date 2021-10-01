#include "Pigache.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Pigache::Pigache ( void ) 
  : Problem ( "PIGACHE" , "PIGACHE" , "xe.txt" , 4 , 12 ) {

  set_bbit ( 3 , NOMAD::INTEGER );

  set_bbot (  0 , NOMAD::OBJ );
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

  NOMAD::Point lb ( 4 , 0.0001 );
  lb[3] = 1;

  NOMAD::Point ub ( 4 , 0.01 );
  ub[2] = 0.002;
  ub[3] = 200;

  set_bounds ( lb , ub );

  NOMAD::Point x0 ( 4 , 0.00505 );
  x0[2] = 0.00105;
  x0[3] = 100;
 
  set_x0 ( x0 );

  set_f_lb ( -99 );

  add_keyword ( "published"        );
  add_keyword ( "real_application" );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Pigache::eval_x ( NOMAD::Eval_Point & xx         ,
		       bool              & count_eval   ) const {

  const double PI      = 3.141592655;
  const double qmin    = 0.8;
  const double qmax    = 1.2;
  const double epsi0   = 8.85e-12;
  const double freqmin = 20.e3;
  const double freqmax = 2.e6;
  const double keffmin = 0.1;
  const double Nmin    = 40;
  const double Nmax    = 60;
  const double dens    = 7.70e3;
  const double d31     = -1.28e-10;
  const double k31     = 0.327;
  const double epsi33  = 1.33e3;
  const double s11E    = 1.30e-11;
  const double s11D    = 1.16e-11;
  const double s33D    = 1.05e-11;
  const double g33     = 0.0280;
  const double beta33  = 8.52e7;
  const double Qa      = 3300;
  const double tangphi = 0.003;

  double x[7];

  for ( int i = 1 ; i <= 4 ; ++i )
    x[i] = xx[i-1].value();

  x[5] = 1000000;
  x[6] = 5;

  double puls0    = (PI/x[2])*sqrt(1.0/(dens*s11E));
  double Z0       = x[3]*x[1]*sqrt(dens/s11D);
  double Cd1      = (x[4]*x[4]*x[2]*x[1]*epsi33*epsi0*(1-k31*k31))/(x[3]);
  double Cd2      = x[3]*x[1]/(x[2]*(beta33+g33*g33/s33D));
  double Rd1      = 1.0/(puls0*Cd1*tangphi);
  double Rd2      = 1.0/(puls0*Cd2*tangphi);
  double psi1     = x[1]*d31/s11E;
  double psi2     = g33/s33D*Cd2;
  double N1       = x[4]*x[4]*psi2/psi1;
  double Lm0      = (PI/4.0*(Z0/puls0));
  double Cm0      = 1.0/(Lm0*puls0*puls0);
  double Rm0      = (1.0/Qa)*sqrt(Lm0/Cm0);
  double Cm       = Cm0*(psi1*psi1*x[4]*x[4]);
  double Lm       = Lm0/(psi1*psi1*x[4]*x[4]);
  double Rm       = Rm0/(psi1*psi1*x[4]*x[4]);
  double Req      = Rd2*x[5]/(x[5]+Rd2);
  double qi       = Cd2*Req*puls0;
  double Ceq      = (Cd2*(N1*N1)*Cm*(1.0+qi*qi))/((qi*qi)*Cm+(N1*N1)*Cd2*(1.0+qi*qi));
  double freqres  = puls0/(2.0*PI);
  double freqanti = 1.0/(2.0*PI*sqrt(Lm*(Cd1*Ceq/(Cd1+Ceq))));
  double keff     = sqrt(1.0-Cd1*Ceq/((Cd1+Ceq)*Cm));
  double Xeq      = Cd2*puls0;
  
  double eta = 100.0*Req*Req/x[5]*Cm*Cm*puls0*puls0*Rd1*N1*N1/((1.0+Req*Req*Xeq*Xeq)*(Cm*Cm*puls0*puls0*(Lm*Lm*puls0*puls0+Rm*(Rm+Rd1))-2.0*Cm*Lm*puls0*puls0+1.0)*N1*N1*N1*N1+(2.0*Req*Req*Xeq*Cm*puls0*(1.0-Cm*Lm*puls0*puls0)+Req*Cm*Cm*puls0*puls0*(Rd1+2.0*Rm))*N1*N1+Req*Req*Cm*Cm*puls0*puls0);

  double z = -eta;;

  double g1  = qmin-qi;
  double g2  = qi-qmax;
  double g3  = freqmin-freqres;
  double g4  = freqres-freqmax;
  double g5  = freqres-freqanti;
  double g6  = Nmin-sqrt(N1*N1);
  double g7  = sqrt(N1*N1)-Nmax;
  double g8  = keffmin-keff;
  double g9  = (x[1]/(2.0*x[2]))-1.0;
  double g10 = -Cd1;
  double g11 = -Cd2;

  xx.set_bb_output ( 0 , z );

  xx.set_bb_output (  1 , g1  );
  xx.set_bb_output (  2 , g2  );
  xx.set_bb_output (  3 , g3  );
  xx.set_bb_output (  4 , g4  );
  xx.set_bb_output (  5 , g5  );
  xx.set_bb_output (  6 , g6  );
  xx.set_bb_output (  7 , g7  );
  xx.set_bb_output (  8 , g8  );
  xx.set_bb_output (  9 , g9  );
  xx.set_bb_output ( 10 , g10 );
  xx.set_bb_output ( 11 , g11 );

  count_eval = true;
  
  return true;
}
