#include <iomanip>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
using namespace std;

// #define DEBUG

// constantes :
const long double EPSILON     = 1e-8;
const unsigned    maxit       = 100;
const int         it          = 5000;
const long double G           = 3900000.0;
const long double density     = 176.0;
const long double W0          = 6000.0;
const long double sigma_allow = 73200.0;
const long double tau_allow   = 47900.0;
const long double alt         = 30000.0;
const long double Mach        = 0.75;
const long double PI          = 3.1415926535;
const long double power_P_isa = 5.2561;
const long double T_isa       = 518.7 - ( 3.5662 * alt * 0.001 ); //Rankine
const long double P_isa       = 14.7 * pow ( 1.0 - alt * 0.001 / 145.45 , power_P_isa ); //psi
const long double rho         = P_isa * 144.0 / ( T_isa * 1716.0 ); //slug/ft2
const long double a           = sqrt ( 1.4 * 1716 * T_isa );
const long double q           = 0.5 * rho * Mach * a * Mach * a;

// variables globales :
long double l   [5001];
long double Rl  [5001];
long double d   [5001];
long double Rd  [5001];
long double T   [5001];
long double del [5001];

/*-----------------------------------------------------------------*/
/*                          objective function                     */
/*-----------------------------------------------------------------*/
long double eval_f ( long double L , long double D , long double Wf , long double Ws ) {
  return - L * log( (W0 + Wf + Ws ) / (W0 + Ws) ) / D;
}

/*-----------------------------------------------------------------*/
/*                             constraint 1                        */
/*-----------------------------------------------------------------*/
long double eval_g1 ( long double L , long double Wf , long double Ws ) {
  return W0 + Wf + Ws - L;
}

/*-----------------------------------------------------------------*/
/*                             constraint 2                        */
/*-----------------------------------------------------------------*/
long double eval_g2 ( long double sigma_max ) {
  return sigma_max - sigma_allow;
}

/*-----------------------------------------------------------------*/
/*                             constraint 3                        */
/*-----------------------------------------------------------------*/
long double eval_g3 ( long double tau_max ) {
  return tau_max - tau_allow;
}

/*-----------------------------------------------------------------*/
/*                            AERO discipline                      */
/*-----------------------------------------------------------------*/
void aero ( long double b , long double Cr , long double lambda , long double alpha_r ,
	    long double alpha0_t , long double delta_alpha ,
	    long double & L , long double & D ,
	    long double & clr , long double & clt ,
	    long double & cdr , long double & cdt,
	    long double & cmr , long double & cmt   ) {

  // AERO 2D :
  // ---------

  clr = 0.3 + 0.2 * alpha_r;
  cdr = 0.009 + 0.0005 * alpha_r * alpha_r;
  cmr = -0.09 - 0.006 * alpha_r * alpha_r;

  long double alpha_t = alpha0_t + delta_alpha;
        
  clt = 0.3 + 0.2 * alpha_t;
  cdt = 0.009 + 0.0005 * alpha_t * alpha_t;
  cmt = -0.09 - 0.006 * alpha_t * alpha_t;


  // AERO 3D :
  // ---------

  // Calcul de la portance et de la trainee :
  L = q*Cr*b*(PI*(5.0*clr-clt-clr*lambda+clr*lambda)/32.0+(clr*lambda+clt-2.0*clr)/6.0);

  // Trainee :
  D = q*b*Cr*(2.0*cdt*lambda+cdt+cdr*lambda+2.0*cdr)/12.0;
}

/*-----------------------------------------------------------------*/
/*                           STRUC discipline                      */
/*-----------------------------------------------------------------*/
void struc ( long double b , long double Cr , long double lambda , long double dr , long double tr ,
	     long double clr , long double clt , long double cdr , long double cdt ,
	     long double cmr , long double cmt ,
	     long double & delta_alpha , long double & Wf , long double & Ws ,
	     long double & sigma_max , long double & tau_max ) {

  int i;
  long double del_y=b/2.0/it; // Pas suivant y
  long double yci , l_y , d_y , m_y , J_y , r_y , r_y_i;
  

  l[0]=Rl[0]=0.0;
  d[0]=Rd[0]=0.0;
  T[0]=0.;

  for (i = 1; i <= it; i++ ) 
    {
      yci = b/2.0-del_y*i+del_y/2.0;
      l_y = q*(clr*b+2.0*yci*clt-2.0*yci*clr)/pow(b,2.0L)*Cr*(b+2.0*yci*lambda-2.0*yci)
	*sqrt((pow(b,2.0L)-4*pow(yci,2.0L))/pow(b,2.0L));

      d_y = q*(cdr*b+2.0*yci*cdt-2.0*yci*cdr)*Cr*(b+2.0*yci*lambda-2.0*yci)/pow(b,2.0L);
      d[i] = d[i-1]+l_y*del_y;
      Rd[i] = ((Rd[i-1]+del_y)*d[i-1]+pow(del_y,2.0L)/2.0*d_y)/d[i];

      l[i] = l[i-1]+l_y*del_y;
      Rl[i] = ((Rl[i-1]+del_y)*l[i-1]+pow(del_y,2.0L)/2.0*l_y)/l[i];

      m_y = q*(cmr+2.0/b*(cmt-cmr)*yci)*pow(Cr+2.0/b*Cr*(lambda-1.0)*yci,2.0L);
      T[i] = m_y*del_y+T[i-1];
    }

  del[it]=0.0;
  for (i = it-1; i >= 1; i-- ) //À l'envers
    {
      yci    = b/2.-del_y*i+del_y/2.;
      r_y    = dr/2.0+2.0/b*dr/2.0*(lambda-1.0)*yci;
      r_y_i  = (dr/2.0-tr)+2.0/b*(dr/2.0-tr)*(lambda-1.0)*yci;
      J_y    = PI/2.0*(pow(r_y,4.0L)-pow(r_y_i,4.0L));
      del[i] =-T[i]*del_y/G/J_y*180.0/PI+del[i+1];
    }

  long double tauyz;
  long double tauyx;
  long double tauyt;
  long double sigmay;
	
  long double tauy;
  long double sigmamin;

  long double taumax_tmp;
  long double sigmamax_tmp;

  long double temp , Aire , I_y;

  sigma_max = tau_max = 0.0;

  for (i = 1; i <= it; i++ ) 
    {
      yci = b/2.-del_y*i+del_y/2.;
      r_y = dr/2.0+2.0/b*dr/2.0*(lambda-1.0)*yci;
      r_y_i = (dr/2.0-tr)+2.0/b*(dr/2.0-tr)*(lambda-1.0)*yci;
      J_y = PI/2.0*(pow(r_y,4.0L)-pow(r_y_i,4.0L));
      Aire = PI*(pow(r_y,2.0L)-pow(r_y_i,2.0L));
      tauyz = l[i]/Aire; //Portance
      tauyx = d[i]/Aire; //Trainee
      tauyt = fabs(T[i]*r_y/J_y);//Torsion
      I_y = PI/4.0*(pow(r_y,4.0L)-pow(r_y_i,4.0L));
      sigmay = (Rl[i]*l[i]+Rd[i]*d[i])*r_y/I_y;//approx.
      tauy = sqrt(pow(tauyz,2.0L)+pow(tauyx,2.0L)+tauyt);
      temp = sqrt(pow(sigmay,2.0L)/4.0+pow(tauy,2.0L));
      sigmamax_tmp = sigmay/2.0+temp;
      sigmamin = sigmay/2.0-temp;
      taumax_tmp = fabs(sigma_max-sigmamin)/2.0;
           
      if (sigmamax_tmp > sigma_max)
	sigma_max = sigmamax_tmp;
           
      if (taumax_tmp > tau_max)
	tau_max = taumax_tmp;
    }

  Ws = density*3.1416*b/6.0*(1+lambda+(lambda*lambda))*(pow(dr/2.0,2)-pow(dr/2.0-tr,2)) ;
  Wf = 20.0*12.0/10.0*b/4.0*12.0*(1.0+lambda);
  
  delta_alpha = del[1];
}

/*-----------------------------------------------------------------*/
/*          MDA Analysis : resolution by fixed point method        */
/*-----------------------------------------------------------------*/
bool MDA_analysis (  long double b , long double Cr , long double lambda , long double alpha_r ,
		     long double alpha0_t , long double dr , long double tr ,
		     long double & L , long double & D , long double & Wf , long double & Ws ,
		     long double & sigma_max ,  long double & tau_max ) {

  Wf = 1000;
  Ws = 1000;
  sigma_max = 1000;
  tau_max = 1000;


  long double delta_alpha = 0.1L;

  long double clr , clt , cdr , cdt , cmr , cmt , delta_alpha2;

  unsigned itcnt = 0;

  while(1) {

    aero ( b , Cr , lambda , alpha_r , alpha0_t , delta_alpha ,
	   L , D , clr , clt , cdr , cdt , cmr , cmt );

    struc ( b , Cr , lambda , dr , tr ,  clr , clt , cdr , cdt , cmr , cmt ,
	    delta_alpha2 , Wf , Ws , sigma_max ,  tau_max );

    if ( (fabs(delta_alpha2-delta_alpha) < EPSILON) || (itcnt==maxit) )
      break;
    
    delta_alpha = delta_alpha2;

    itcnt++;
  }

#ifdef DEBUG
  cout.setf(ios::fixed);
  cout << setprecision(20);
  cout << endl;
  cout << "L = " << L << endl
       << "D = " << D << endl
       << "clr = " << clr << endl
       << "cdr = " << cdr << endl
       << "cmr = " << cmr << endl
       << "clt = " << clt << endl
       << "cdt = " << cdt << endl
       << "cmt = " << cmt << "\n\n";
  cout << "sigmamax = " << sigma_max << endl
       << "taumax = " << tau_max << endl
       << "Wf = " << Wf << endl
       << "Ws = " << Ws << endl
       << "delta_alpha = " << delta_alpha << endl
       << "iterations = " << itcnt << "\n\n";
#endif

  if (itcnt==maxit)
    return false;

  return true;
}

/*-----------------------------------------------------------------*/
/*                              read inputs                        */
/*-----------------------------------------------------------------*/
bool read_inputs ( const string & name     ,
		   long double  & b        ,
		   long double  & Cr       ,
		   long double  & lambda   ,
		   long double  & alpha_r  ,
		   long double  & alpha0_t ,
		   long double  & dr       ,
		   long double  & tr         ) {
  ifstream in (name.c_str());

  in >> b >> Cr >> lambda >> alpha_r >> alpha0_t >> dr >> tr;

  if (in.fail()) {
    cerr << "\nerror with file " << name << "\n\n";
    return false;
  }

  in.close();
  return true;
}

/*-----------------------------------------------------------------*/
/*                              display outputs                    */
/*-----------------------------------------------------------------*/
void write_outputs ( long double f , long double g1 , long double g2 ,
		     long double g3 , ostream & out ) {

  out.setf(ios::fixed);
  out << setprecision(20);

#ifdef DEBUG

  out << "\nf="  << f
      << "  g1=" << g1
      << "  g2=" << g2
      << "  g3=" << g3 << "\n\n";

  long double eps = 1e-6L;

  if (g1>eps || g2>eps || g3>eps)
    out << "Constraint Violation\n\n";

#else

  out << f << " " << g1 << " " << g2 << " " << g3 << endl;

#endif

}

/*-----------------------------------------------------------------*/
/*                            main function                        */
/*-----------------------------------------------------------------*/
int main ( int argc , char ** argv ) {

  // check args:
  if (argc < 2) {
    cerr << "\nerror : too few arguments (usage: truth.exe  input_file  output_file)\n\n";
    return 1;
  }

  // inputs:
  long double b , Cr , lambda , alpha_r , alpha0_t , dr , tr;
  if (!read_inputs ( argv[1] , b , Cr , lambda , alpha_r , alpha0_t , dr , tr ))
    return 1;

  // MDA analysis:
  long double L , D , Wf , Ws , sigma_max , tau_max;
  bool mda_ok = MDA_analysis ( b , Cr , lambda , alpha_r , alpha0_t , dr , tr ,
			       L , D , Wf , Ws , sigma_max , tau_max );

  // objective function and constraints:
  long double f=1e+20 , g1=1e+20 , g2=1e+20 , g3=1e+20;
  if (mda_ok) {
    f = eval_f  ( L , D , Wf , Ws );
    g1 = eval_g1 ( L , Wf , Ws );
    g2 = eval_g2 ( sigma_max );
    g3 = eval_g3 ( tau_max );
  }

  // outputs display:
  if (argc >= 3) {
    ofstream out (argv[2]);
    if (out.fail()) {
      cerr << "\nerror with file " << argv[2] << "\n\n";
      return 1;
    }
    write_outputs(f,g1,g2,g3,out);
  }
  else
    write_outputs(f,g1,g2,g3,cout);

  return 0;
}
