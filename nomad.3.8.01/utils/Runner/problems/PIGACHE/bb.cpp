#include <cmath>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

const double PI      = 3.141592655;
const double qmin    = 0.8;
const double qmax    = 1.2;
const double epsi0   = 8.85e-12;
const double Hmax    = 2.e-3;
const double freqmin = 20.e3;
const double freqmax = 2.e6;
const double etamin  = 1.e-3;
const double etamax  = 100;
const double keffmin = 0.1;
const double keffmax = 1;
const double Nmin    = 40;
const double Nmax    = 60;
const double Veff    = 4;
const double Tempmax = 200;
const double hconv   = 15;
const double dens    = 7.70e3;
const double d31     = -1.28e-10;
const double k31     = 0.327;
const double k33     = 0.684;
const double epsi33  = 1.33e3;
const double s11E    = 1.30e-11;
const double s11D    = 1.16e-11;
const double s33D    = 1.05e-11;
const double g33     = 0.0280;
const double beta33  = 8.52e7;
const double sigma   = 0.334;
const double Qa      = 3300;
const double tangphi = 0.003;

int main ( int argc , char ** argv ) {

  int      i;
  double   x[7] , c[12];
  bool     err = false;
  ifstream in;

  if ( argc >= 2 ) {
    
    in.open ( argv[1] );

    for ( i = 1 ; i <= 4 ; ++i )
      in >> x[i];

    x[5] = 1000000;
    x[6] = 5;
    
    if ( in.fail() ) {
      err = true;
      for ( i = 0 ; i <= 11 ; ++i )
	c[i] = 1e20;
    }
  }
  else {
    err = true;
    for ( i = 0 ; i <= 11 ; ++i )
      c[i] = 1e20;
  }

  in.close();

  if ( !err ) {
    
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

    // objective:
    c[ 0] = -eta;
    
    // constraints:
    c[ 1] = qmin-qi;
    c[ 2] = qi-qmax;
    c[ 3] = freqmin-freqres;
    c[ 4] = freqres-freqmax;
    c[ 5] = freqres-freqanti;
    c[ 6] = Nmin-sqrt(N1*N1);
    c[ 7] = sqrt(N1*N1)-Nmax;
    c[ 8] = keffmin-keff;
    c[ 9] = (x[1]/(2.0*x[2]))-1.0;
    c[10] = -Cd1;
    c[11] = -Cd2;
  }

  cout << setprecision(15);
  for ( i = 0 ; i <= 11 ; ++i )
    cout << c[i] << " "; // obj: c[0]
  cout << endl;
  
  return 0;
}
