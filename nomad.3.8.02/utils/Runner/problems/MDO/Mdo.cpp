#include "Mdo.hpp"

/*----------------------------------------------*/
/*                  constructor                 */
/*----------------------------------------------*/
Mdo::Mdo ( void ) 
  : Problem ( "MDO" , "MDO" , "xe.txt" , 10 , 11 ) {

  set_bbot (  0 , NOMAD::OBJ );
  set_bbot (  1 , NOMAD::EB  );
  set_bbot (  2 , NOMAD::EB  );
  set_bbot (  3 , NOMAD::EB  );
  set_bbot (  4 , NOMAD::PB  );
  set_bbot (  5 , NOMAD::PB  );
  set_bbot (  6 , NOMAD::PB  );
  set_bbot (  7 , NOMAD::PB  );
  set_bbot (  8 , NOMAD::PB  );
  set_bbot (  9 , NOMAD::PB  );
  set_bbot ( 10 , NOMAD::PB  );

  set_bounds ( NOMAD::Point ( 10 , 0.0 ) , NOMAD::Point ( 10 , 100.0 ) );

  NOMAD::Point x0 ( 10 );
  x0[0] = 100;
  x0[1] = 50;
  x0[2] = 24.4;
  x0[3] = 38.1;
  x0[4] = 50;
  x0[5] = 50;
  x0[6] = 82;
  x0[7] = 11.6;
  x0[8] = 75.6;
  x0[9] =50;

  set_x0 ( x0 );

  set_f_lb ( -3970.0 );

  add_keyword ( "published"        );
  add_keyword ( "real_application" );
  add_keyword ( "orthomads_paper"  );
  add_keyword ( "mads_dfo_paper"   );
}

/*----------------------------------------------*/
/*                   evaluation                 */
/*----------------------------------------------*/
bool Mdo::eval_x ( NOMAD::Eval_Point & xx         ,
		   bool              & count_eval   ) const {

  int    i;
  double inputs[10];
  
  for ( i = 0 ; i < 10 ; i++ )
    inputs[i] = MDO_LB[i] + MDO_RANGE[i] * xx[i].value() / 100.0;
 
  // output variables for MDA:
  double Str[5] , Apg , SFC , ESF , Tb , TbUA , Temp , Range;

  // MDA (fixed points method):
  double eps    = 1e-12;
  int    max_it = 100;

  MDA ( inputs , Str , Apg , SFC , ESF , Tb , TbUA ,
	Temp , Range , eps , max_it ); 

  xx.set_bb_output (  0 , -Range        );
  xx.set_bb_output (  1 , Str[0] - 1.09 );
  xx.set_bb_output (  2 , Str[1] - 1.09 );
  xx.set_bb_output (  3 , Str[2] - 1.09 );
  xx.set_bb_output (  4 , Str[3] - 1.09 );
  xx.set_bb_output (  5 , Str[4] - 1.09 );
  xx.set_bb_output (  6 , Apg - 1.04    );
  xx.set_bb_output (  7 , 0.5 - ESF     );
  xx.set_bb_output (  8 , ESF - 1.5     );
  xx.set_bb_output (  9 , Tb - TbUA     );
  xx.set_bb_output ( 10 , Temp - 1.02   );

  count_eval = true;
  
  return true;
}

/*-------------------------------------------------*/
/*          MDA analysis (fixed point method)      */
/*-------------------------------------------------*/
int Mdo::MDA ( double   inputs[10] ,
	       double   Str   [5 ] ,
	       double & Apg        ,
	       double & SFC        ,
	       double & ESF        ,
	       double & Tb         ,
	       double & TbUA       ,
	       double & Temp       ,
	       double & Range      ,
	       double   eps        ,
	       int      max_it       ) const {

  double delta1 = 0.0 , delta2;
  int         it     = 0;

  double We    = 6550;     // 2000;     //  6550;
  double L     = 46231;    // 50000;    // 46231;
  double Wt    = 41195;    // 50000;    // 41195;
  double Wf    = 11254;
  double theta = 1.02;      // 2.0;      // 1.02;
  ESF               = 0.536;      // 2.0;      // 0.536;
  double D     = 5264;     // 5000;     // 5264;
  double LD    = 9.5;

  while (1) {

    // AERO
    Aero ( inputs , Wt , theta , ESF ,
	   L , D , LD , Apg );

    // PROP
    Prop ( inputs , D ,
	   SFC , We , ESF , Tb , TbUA , Temp );

    // PROP
    Struc ( inputs , We , L ,
	    Wt , Wf , theta , Str );

    delta2 = fabs(We) + fabs(L) + fabs(Wt) + fabs(theta) + fabs(ESF) + fabs(D);

    it++;

    if ( fabs( (delta2 - delta1) / delta2 ) < eps || it >= max_it )
      break;

    delta1 = delta2;
  }

  // no convergence :
  if (it >= max_it) {
    Str[0] = Str[1] = Str[2] = Str[3] = Str[4] = Apg = SFC = Tb = Temp = 1e+20;
    ESF    = Range = -1e+20;
    TbUA   = 0.0;
    return it;
  }

  // compute range :
  double  h = inputs[5];
  double  M = inputs[6];
  double th = ( h < 36089 ) ? (1 - h * 6.875e-6) : 0.7519;

  Range = M * LD * 661.0 * pow(th,0.5) * log ( Wt / (Wt-Wf) ) / SFC;

  return it;
}

/*-------------------------------------------------*/
/*                  discipline STRUC               */
/*-------------------------------------------------*/
void Mdo::Struc ( double   inputs[10] ,
		  double   We         ,
		  double   L          ,
		  double & Wt         ,
		  double & Wf         ,
		  double & theta      ,
		  double   Str   [5 ]   ) const {

  double tc     = inputs[4];
  double AR     = inputs[7];
  double Sref   = inputs[9];
  double lambda = inputs[0];
  double x      = inputs[1];
  double LAMBDA = inputs[8];

  double Wfo=2000;                      // misc. fuel weight lb
  double Wo=25000;                      // misc. weight lb
  double Nz=6;                          // Load case
  double tc_init=0.05;                  // Initial thickness to chord ratio none
  double x_init=1.0;
  double b2_init=pow(5.5*1000.0,0.5)/2.0;   // Initial halfspan length ft
  double R_init=(1.0+2.0*0.25)/(3.0*(1.0+0.25));// Initial location of lift as fraction of halfspan none
  double L_init=50000.0;                // Initial lift lb

  double S_init_1 [4] = {x_init,b2_init,R_init,L_init};
  double S_init_2 [1] ={x_init};
  double flag_1 [4] ={2.0,4.0,4.0,3.0};
  double flag_2 [1] ={1.0};
  double bound_1[4] ={0.25,0.25,0.25,0.25};
  double bound_2[1] ={0.008};
  double t =tc*Sref/pow(Sref*AR,0.5);     // wing thickness
  double b2=pow(Sref*AR,0.5)/2.0;           // wing semi span
  double R=(1.0+2.0*lambda)/(3.0*(1.0+lambda));   // wing aerodynamic center location
  double S_1 [4] = {x,b2,R,L};
  theta=pf(S_init_1,S_1,flag_1,bound_1,4);    // wing twist
  double S_2 [1] ={x};
  double Fo=pf(S_init_2,S_2,flag_2,bound_2,1);
  double Ww=Fo*(0.0051*pow(L*Nz,0.557))*pow(Sref,0.649)*pow(AR,0.5)*pow(tc,-0.4)*(pow(1.0+lambda,0.1))*
    pow(cos(LAMBDA*MDO_PI/180.0),-1.0)*pow(0.1875*Sref,0.1);
  double Wwf=5.0*Sref/18.0*2.0/3.0*t*42.5;
  Wf=Wfo+Wwf;
  Wt=Wo+Ww+Wf+We;

  double S_init_3 [] = {tc_init,L_init,x_init,b2_init,R_init};
  double S_3 [] = {tc,L,x,b2,R};
  double flag_3 [] = {4,1,4,1,1};
  double bound_3 [] = {0.1,0.1,0.1,0.1,0.1};
  Str[0]=pf(S_init_3,S_3,flag_3,bound_3,5);

  double S_init_4 [] = {tc_init,L_init,x_init,b2_init,R_init};
  double S_4 [] = {tc,L,x,b2,R};
  double flag_4 [] = {4,1,4,1,1};
  double bound_4 [] = {0.15,0.15,0.15,0.15,0.15};
  Str[1]=pf(S_init_4,S_4,flag_4,bound_4,5);

  double S_init_5 [] = {tc_init,L_init,x_init,b2_init,R_init};
  double S_5 [] = {tc,L,x,b2,R};
  double flag_5 [] = {4,1,4,1,1};
  double bound_5 [] = {0.2,0.2,0.2,0.2,0.2};
  Str[2]=pf(S_init_5,S_5,flag_5,bound_5,5);

  double S_init_6 [] = {tc_init,L_init,x_init,b2_init,R_init};
  double S_6 [] = {tc,L,x,b2,R};
  double flag_6 [] = {4,1,4,1,1};
  double bound_6 [] = {0.25,0.25,0.25,0.25,0.25};
  Str[3]=pf(S_init_6,S_6,flag_6,bound_6,5);

  double S_init_7 [] = {tc_init,L_init,x_init,b2_init,R_init};
  double S_7 [] = {tc,L,x,b2,R};
  double flag_7 [] = {4,1,4,1,1};
  double bound_7 [] = {0.25,0.25,0.25,0.25,0.25};
  Str[4]=pf(S_init_7,S_7,flag_7,bound_7,5);
}

/*-------------------------------------------------*/
/*                  discipline AERO                */
/*-------------------------------------------------*/
void Mdo::Aero ( double   inputs [10] ,
		 double   Wt          ,
		 double   theta       ,
		 double   ESF         ,
		 double & L           ,
		 double & D           ,
		 double & LD          ,
		 double & Apg           ) const {

  double Cf     = inputs[2];
  double tc     = inputs[4];
  double h      = inputs[5];
  double M      = inputs[6];
  double AR     = inputs[7];
  double LAMBDA = inputs[8];
  double Sref   = inputs[9];

  double CDmin_M1=0.01375;               // Min drag coeff M<1
  double Cf_init=1.0;                    // Initial friction coeff. none
  double ESF_init=1.0;                   // Initial engine scale factor none
  double tc_init=0.05;                   // Initial thickness to chord ratio none
  double theta_init=1.0;                 // Initial wing twist p.f.
  double CL;                             // Coefficient of lift none
  double CD;                             // Coefficient of drag none
  double CDmin;                          // Minimum drag coefficient none
  double k;                              // Induced drag factor none
  double rho;                            // Density slug/ft3
  double V;                              // Velocity ft/s

  if (h<36089)
    {
      V = M*1116.39*pow(1-6.875E-06*h,0.5);
      rho = 2.377E-03*pow(1-6.875e-06*h,4.2561);
    }
  else
    {
      V = M*968.1;
      rho = 2.377e-03*0.2971*exp(-(h-36089.0)/20806.7);
    }
   
  CL = Wt/(0.5*rho*pow(V,2.0)*Sref);

  //-----Polynomial function modifying CDmin for ESF and friction coefficient-----%
  
  double S_init_1 [2]= {ESF_init,Cf_init};
  double S_1 [2] = {ESF,Cf};
  double flag_1 [2] = {1.0,1.0};
  double bound_1 [2] = {0.25,0.25};
  double Fo1 = pf(S_init_1,S_1,flag_1,bound_1,2);
  CDmin = CDmin_M1*Fo1 + 3.05*pow(tc,5.0/3.0)*pow(cos(LAMBDA*MDO_PI/180.0),1.5);
   
  if (M >= 1)
    {
      k=AR*(pow(M,2.0)-1.0)*cos(LAMBDA*MDO_PI/180.0)/(4.0*AR*pow(pow(LAMBDA,2.0)-1.0,0.5)-2.0);
    }
  else
    {
      k=1.0/(MDO_PI*0.8*AR);
    }
  
  //-----Polynomial function modifying CD for wing twist-----//
  
  double S_init_2 [] = {theta_init};
  double S_2 [] = {theta};
  double flag_2 [] = {5.0};
  double bound_2 [] = {0.25};
  double Fo2=pf(S_init_2,S_2,flag_2,bound_2,1);
  
  CD = Fo2*(CDmin + k*pow(CL,2.0));
  D = 0.5*rho*pow(V,2.0)*CD*Sref;
  
  //------Lift and Lift-to-drag---------//
  L = Wt;
  LD = CL/CD;
      
  double S_init_3 [] = {tc_init};
  double S_3 [] = {tc};
  double flag_3 [] = {1.0};
  double bound_3 [] = {0.25};
  
  Apg = pf(S_init_3,S_3,flag_3,bound_3,1); //--adverse pressure gradient
}

/*-------------------------------------------------*/
/*                  discipline PROP                */
/*-------------------------------------------------*/
void Mdo::Prop ( double   inputs [10] ,
		 double   D           ,
		 double & SFC         ,
		 double & We          ,
		 double & ESF         ,
		 double & Tb          ,
		 double & TbUA        ,
		 double & Temp          ) const {

  double T = inputs[3];
  double h = inputs[5];
  double M = inputs[6];

  double Wbe = 4360;
  double h_init=45000;                  // Initial altitude
  double T_init=0.5;                    // Initial throttle setting none
  double Mach_init=1.6;                 // Initial Mach number none

  Tb = T*16168.6; //--non-diminsional throttle setting
  //-----Surface fit to engine deck (obtained using least squares approx)-----%
  double s [] = {1.13238425638512,1.53436586044561,-0.00003295564466,-0.00016378694115,-0.31623315541888,0.00000410691343,-0.00005248000590,-0.00000000008574,0.00000000190214,0.00000001059951};
  
  
  SFC=s[0]+s[1]*M+s[2]*h+s[3]*Tb+s[4]*pow(M,2.0)+2.0*h*M*s[5]+2.0*Tb*M*s[6]+s[7]*pow(h,2.0)+2.0*Tb*h*s[8]+s[9]*pow(Tb,2.0);
    
  ESF = (D/3.0)/Tb;
  We = 3.0*Wbe*pow(ESF,1.05);

  //---THIS SECTION COMPUTES POLYNOMIAL CONSTRAINT FUNCTIONS--%
  double S_init_1 [] = {Mach_init,h_init,T_init};
  double S_1 [] = {M,h,T};
  double flag_1 [] = {2.0,4.0,2.0};
  double bound_1 [] = {0.25,0.25,0.25};
  Temp = pf(S_init_1,S_1,flag_1,bound_1,3); 
  double p [] = {11483.7822254806,10856.2163466548,-0.5080237941,3200.157926969,-0.1466251679,0.0000068572};
  TbUA=p[0]+p[1]*M+p[2]*h+p[3]*pow(M,2.0)+2.0*p[4]*M*h+p[5]*pow(h,2.0);
}

/*------------------------------------------------------------------*/
/*                        polynomial function                       */
/*------------------------------------------------------------------*/
double Mdo::pf ( double S[],double S_new[],
		 double flag[], double S_bound[],int dim) const {
   double *S_norm=new double [dim];
   double *S_shifted=new double [dim];
   double a,b,So,Sl,Su,ff,f_tmp;
   double Ao=0.0;
   double *Ai=new double [dim];
   double (*Aij)[20]=new double [20][20];
   MDO_matrix Mtx_shifted(3,3);
   MDO_vect F_bound(3),A(3);
   for (int i=0;i<dim;i++)
   {
      S_norm[i]=S_new[i]/S[i];
      if (S_norm[i]>1.25)
         S_norm[i]=1.25;
      if (S_norm[i]<0.75)
            S_norm[i]=0.75;
      S_shifted[i]=S_norm[i]-1.0;

      // Determine bounds on FF depending on slope-shape
      a=0.1;
      b=a;
      if (flag[i]==5.0)
      {
         // Calculate polynomial coefficient (S-about origin)
         So=0;
         Sl=So-S_bound[i];
         Su=So+S_bound[i];        
         Mtx_shifted.set_elem(0,0,1.0);
         Mtx_shifted.set_elem(0,1,Sl);
         Mtx_shifted.set_elem(0,2,pow(Sl,2.0));
         Mtx_shifted.set_elem(1,0,1.0);
         Mtx_shifted.set_elem(1,1,So);
         Mtx_shifted.set_elem(1,2,pow(So,2.0));
         Mtx_shifted.set_elem(2,0,1.0);
         Mtx_shifted.set_elem(2,1,Su);
         Mtx_shifted.set_elem(2,2,pow(Su,2.0));
         F_bound.set_elem(0,1.0+pow(0.5*a,2.0));
         F_bound.set_elem(1,1.0);
         F_bound.set_elem(2,1.0+pow(0.5*b,2.0));
         A=Mtx_shifted.inverse(F_bound);
         Ao=A.get_elem(0);
         Ai[i]=A.get_elem(1);
         Aij[i][i]=A.get_elem(2);

      }
      else
      {
         switch (int(flag[i]))
         {
         case 1 :
            break;
         case 0 :
            S_shifted[i]=0.0;
            break;
         case 3 :
            a=-a;
            b=a;
            break;
         case 2 :
            b=2.0*a;
            break;
         case 4 :
            a=-a;
            b=2.0*a;
            break;
         default :
	   return 1e20;
         }
      
         // Calculate polynomial coefficient (S-about origin)
         So=0.0;
         Sl=So-S_bound[i];
         Su=So+S_bound[i];
         Mtx_shifted.set_elem(0,0,1.0);
         Mtx_shifted.set_elem(0,1,Sl);
         Mtx_shifted.set_elem(0,2,pow(Sl,2.0));
         Mtx_shifted.set_elem(1,0,1.0);
         Mtx_shifted.set_elem(1,1,So);
         Mtx_shifted.set_elem(1,2,pow(So,2.0));
         Mtx_shifted.set_elem(2,0,1.0);
         Mtx_shifted.set_elem(2,1,Su);
         Mtx_shifted.set_elem(2,2,pow(Su,2.0));
         F_bound.set_elem(0,1.0-0.5*a);
         F_bound.set_elem(1,1.0);
         F_bound.set_elem(2,1.0+0.5*b);
         A=Mtx_shifted.inverse(F_bound);
         Ao=A.get_elem(0);
         Ai[i]=A.get_elem(1);
         Aij[i][i]=A.get_elem(2);
      }
   }
   double R[5][5]={{0.2736,0.3970,0.8152,0.9230,0.1108},{0.4252,0.4415,0.6357,0.7435,0.1138},{0.0329,0.8856,0.8390,0.3657,0.0019},{0.0878,0.7248,0.1978,0.0200,0.0169},{0.8955,0.4568,0.8075,0.9239,0.2525}};
   
   
   ff=Ao;
   for (int i=0;i<dim;i++)
   {
      for (int j=i+1;j<dim;j++)
      {
         Aij[i][j]=Aij[i][i]*R[i][j];
         Aij[j][i]=Aij[i][j];
      }
      
   }
   
   for (int i=0;i<dim;i++)
   {
      ff=ff+Ai[i]*S_shifted[i];
      f_tmp=0;
      for (int j=0;j<dim;j++)
      {
         f_tmp=f_tmp+Aij[i][j]*S_shifted[j];
      }
      ff=ff+0.5*f_tmp*S_shifted[i];
   }
   
   delete  S_norm;
   delete  S_shifted;
   delete Ai;
   delete [] Aij;
   return (ff);
}



MDO_vect::MDO_vect() {
   size=0;
   v=0;
}

MDO_vect::MDO_vect(int n) {
   v=new double[n];
   size=n;
}

MDO_vect::MDO_vect(const MDO_vect &vv) {
   size=vv.size;
   v=new double[size];
   for (int i=0;i<size;i++)
      v[i]=vv.v[i];
}

MDO_vect::MDO_vect(int n,double x) {
   size=n;
   v=new double[size];
   for (int i=0;i<size;i++)
      v[i]=x;
}

const MDO_vect &MDO_vect::operator=(const MDO_vect &vv) {
   set_size(vv.size);
   for (int i=0;i<size;i++)
      v[i]=vv.v[i];
   return *this;
}

MDO_vect MDO_vect::operator+(const MDO_vect &vv) {
   MDO_vect tmp(size);
   for (int i=0;i<size;i++)
      tmp.v[i]=v[i]+vv.v[i];
   return tmp;
}

const MDO_vect &MDO_vect::operator+=(const MDO_vect &vv) {
   for (int i=0;i<size;i++)
      v[i]+=vv.v[i];
   return *this;
}

MDO_vect MDO_vect::operator-(const MDO_vect &vv) {
   MDO_vect tmp(size);
   for (int i=0;i<size;i++)
      tmp.v[i]=v[i]-vv.v[i];
   return tmp;
}

const MDO_vect &MDO_vect::operator-=(const MDO_vect &vv) {
   for (int i=0;i<size;i++)
      v[i]-=vv.v[i];
   return *this;
}

const MDO_vect &MDO_vect::operator*=(double aa) {
   for (int i=0;i<size;i++)
      v[i]*=aa;
   return *this;
}

MDO_vect MDO_vect::operator*(double aa) {
   MDO_vect tmp(size);
   for (int i=0;i<size;i++)
      tmp.v[i]=v[i]*aa;
   return tmp;
}
 
double scal(const MDO_vect &v1,const MDO_vect &v2) {
   double tmp=0;
   for (int i=0;i<v1.size;i++)
      tmp+=v1.v[i]*v2.v[i];
   return tmp;
}

double dist(const MDO_vect &v1,const MDO_vect &v2) {
   double tmp=0;
   for (int i=0;i<v1.size;i++)
      tmp+=pow(v1.v[i]-v2.v[i],2);
   return sqrt(tmp);
}

double MDO_vect::norme() {
   double d=0;
   for (int i=0;i<size;i++)
      d+=v[i]*v[i];
   return sqrt(d);
}
 
MDO_vect::~MDO_vect() {
   size=0;
   delete[] v;
}

void MDO_vect::set_size(int n) {
   if (size!=0)
      delete[] v;
   v=new double[n];
   size=n;
}

int MDO_vect::get_size() const {
   return size;
}

void MDO_vect::set_elem(int i,double x) {
   v[i]=x;
}

double MDO_vect::get_elem(int i) const {
   return v[i];
}

double *MDO_vect::get_elem() const {
   return v;
}

void MDO_vect::add2elem(int i,double x) {
   v[i]+=x;
}

MDO_matrix::MDO_matrix() {
   size=0;
   m=0;
}

MDO_matrix::MDO_matrix(int n) {
   m=new double*[n];
   for (int i=0;i<n;i++)
      m[i]=new double[n];
   size=n;
}

MDO_matrix::MDO_matrix(int n,double x) {
   m=new double*[n];
   for (int i=0;i<n;i++)
      m[i]=new double[n];
   size=n;
   for (int i=0;i<n;i++)
      for (int j=0;j<n;j++)
         m[i][j]=x;
}

MDO_matrix::MDO_matrix(const MDO_matrix &mm) {
   size=mm.size;
   m=new double*[size];
   for (int i=0;i<size;i++)
      m[i]=new double[size];
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         m[i][j]=mm.m[i][j];
}

const MDO_matrix &MDO_matrix::operator=(const MDO_matrix &mm) {
   clear();
   size=mm.size;
   m=new double*[size];
   for (int i=0;i<size;i++)
      m[i]=new double[size];
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         m[i][j]=mm.m[i][j];
   return *this;
}

MDO_matrix MDO_matrix::operator+(const MDO_matrix &mm) {
   MDO_matrix tmp(size);
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         tmp.m[i][j]=m[i][j]+mm.m[i][j];
   return tmp;
}

const MDO_matrix &MDO_matrix::operator+=(const MDO_matrix &mm) {
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         m[i][j]+=mm.m[i][j];
   return *this;
}

const MDO_matrix &MDO_matrix::operator*=(double aa) {
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         m[i][j]*=aa;
   return *this;
}

MDO_matrix MDO_matrix::operator*(double aa) {
   MDO_matrix tmp(size);
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         tmp.m[i][j]=m[i][j]*aa;
   return tmp;
}
 
MDO_vect MDO_matrix::operator*(const MDO_vect &vv) {
   MDO_vect tmp(size);
   for (int i=0;i<size;i++) {
      tmp.set_elem(i,0);
      for (int j=0;j<size;j++)
         tmp.add2elem(i,vv.get_elem(j)*m[i][j]);
   }
   return tmp;
}

MDO_matrix MDO_matrix::operator*(const MDO_matrix &mm) {
   MDO_matrix tmp(size);
   double s;
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++) {
         s=0;
         for (int k=0;k<size;k++)
            s+=m[i][k]*mm.m[k][j];
         tmp.set_elem(i,j,s);
      }
   return tmp;
}

MDO_matrix::~MDO_matrix() {
   for (int i=0;i<size;i++)
      delete[] m[i];
   delete[] m;
   size=0;
}

void MDO_matrix::set_size(int n) {
   clear();
   m=new double*[n];
   for (int i=0;i<n;i++)
      m[i]=new double[n];
   size=n;
}

int MDO_matrix::get_size() const {
   return size;
}

void MDO_matrix::set_elem(int i,int j,double x) {
   m[i][j]=x;
}

void MDO_matrix::add2elem(int i,int j,double x) {
   m[i][j]+=x;
}

double MDO_matrix::get_elem(int i,int j) const {
   return m[i][j];
}

void MDO_matrix::diag(double aa,int n) {
   set_size(n);
   for (int i=0;i<size;i++) {
      for (int j=0;j<size;j++)
         m[i][j]=0;
      m[i][i]=aa;
   }
}

void MDO_matrix::ludcmp(int *indx, double *d)
{
  int i,imax=0,j,k;
  double big,dum,sum,temp;
  double *vv=new double[size];
  const double tiny=1e-20;
  
  *d=1.0;
  for (i=0;i<size;i++) {
    big=0.0;
    for (j=0;j<size;j++)
      if ((temp=fabs(m[i][j]))>big)
        big=temp;
    if (big==0) {
      return;
    }
    vv[i]=1.0/big;
  }
  for (j=0;j<size;j++) {
    for (i=0;i<j;i++) {
      sum=m[i][j];
      for (k=0;k<i;k++)
        sum-=m[i][k]*m[k][j];
      m[i][j]=sum;
    }
    big=0.0;
    for (i=j;i<size;i++) {
      sum=m[i][j];
      for (k=0;k<j;k++)
        sum-=m[i][k]*m[k][j];
      m[i][j]=sum;
      if ((dum=vv[i]*fabs(sum))>=big) {
        big=dum;
        imax=i;
      }
    }
    if (j!=imax) {
      for (k=0;k<size;k++) {
        dum=m[imax][k];
        m[imax][k]=m[j][k];
        m[j][k]=dum;
      }
      *d=-(*d);
      vv[imax]=vv[j];
    }
    indx[j]=imax;
    if (m[j][j]==0.0)
      m[j][j]=tiny;
    if (j!=size-1) {
      dum=1.0/(m[j][j]);
      for (i=j+1; i<size; i++)
        m[i][j]*=dum;
    }
  }
}

void MDO_matrix::lubksb(int *indx,MDO_vect &b)
{
   int i,ii=-1,ip,j;
   double sum;
   for (i=0;i<size;i++) {
      ip=indx[i];
      sum=b.get_elem(ip);
      b.set_elem(ip,b.get_elem(i));
      if (ii>=0)
         for (j=ii;j<=i-1;j++)
            sum-=m[i][j]*b.get_elem(j);
      else if (sum)
         ii=i;
      b.set_elem(i,sum);
   }
   for (i=size-1;i>=0;i--) {
      sum=b.get_elem(i);
      for (j=i+1;j<size;j++)
         sum-=m[i][j]*b.get_elem(j);
      b.set_elem(i,sum/m[i][i]);
   }
}

const MDO_vect MDO_matrix::inverse(const MDO_vect &vv) {
   MDO_matrix tmp=*this;
   MDO_vect vtmp=vv;
   double d;
   int *indx=new int[size];
   tmp.ludcmp(indx,&d);
   tmp.lubksb(indx,vtmp);
   delete[] indx;
   return vtmp;
}

const MDO_matrix MDO_matrix::inverse() {
   MDO_matrix tmp=*this,result(size);
   double d;
   int *indx=new int[size];
   tmp.ludcmp(indx,&d);
   for (int j=0;j<size;j++) {
      MDO_vect col(size);
      for (int i=0;i<size;i++)
         col.set_elem(i,0);
      col.set_elem(j,1);
      tmp.lubksb(indx,col);
      for (int i=0;i<size;i++)
         result.set_elem(i,j,col.get_elem(i));
   }
   delete[] indx;
   return result;
}

const MDO_matrix MDO_matrix::transpose() {
   MDO_matrix tmp(size);
   for (int i=0;i<size;i++)
      for (int j=0;j<size;j++)
         tmp.set_elem(i,j,m[j][i]);
   return tmp;
}

void MDO_matrix::clear() {
   if (size!=0) {
      for (int i=0;i<size;i++)
         delete[] m[i];
      delete[] m;
      size=0;
   }
}
