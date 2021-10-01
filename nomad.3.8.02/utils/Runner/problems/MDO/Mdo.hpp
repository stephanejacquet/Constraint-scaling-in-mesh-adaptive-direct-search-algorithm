#ifndef __MDO__
#define __MDO__

#include "../../Problem.hpp"

const double MDO_PI = 3.1415926535;
const double MDO_LB[] = { 0.1 , 0.75 , 0.75 , 0.1 , 0.01 , 30000 ,
			  1.4 , 2.5 , 40 ,  500 };
const double MDO_RANGE[] = { 0.3 , 0.50 , 0.50 , 0.9 , 0.08 , 30000 ,
			     0.4 , 6.0 , 30 , 1000 };

class MDO_vect
{
public:
   MDO_vect();
   MDO_vect(int n);
   MDO_vect(int n,double x);
   MDO_vect(const MDO_vect &vv);
   ~MDO_vect();
   const MDO_vect &operator=(const MDO_vect &vv);
   MDO_vect operator+(const MDO_vect &vv);
   const MDO_vect &operator+=(const MDO_vect &vv);
   MDO_vect operator-(const MDO_vect &vv);
   const MDO_vect &operator-=(const MDO_vect &vv);
   MDO_vect operator*(double aa);
   const MDO_vect &operator*=(double aa);
   friend double scal(const MDO_vect &v1,const MDO_vect &v2);
   friend double dist(const MDO_vect &v1,const MDO_vect &v2);
   void set_size(int n);
   int get_size() const;
   void set_elem(int i,double x);
   double get_elem(int i) const;
   double *get_elem() const;
   void add2elem(int i,double x);
   double norme();
private:
   double *v;
   int size;
};

class MDO_matrix
{
public:
   MDO_matrix();
   MDO_matrix(int n);
   MDO_matrix(int n,double x);
   MDO_matrix(const MDO_matrix &mm);
   ~MDO_matrix();
   const MDO_matrix &operator=(const MDO_matrix &mm);
   MDO_matrix operator+(const MDO_matrix &mm);
   const MDO_matrix &operator+=(const MDO_matrix &mm);
   const MDO_matrix &operator*=(double aa);
   MDO_matrix operator*(double aa);
   MDO_vect operator*(const MDO_vect &vv);
   MDO_matrix operator*(const MDO_matrix &mm);
   void set_size(int n);
   int get_size() const;
   void set_elem(int i,int j,double x);
   void add2elem(int i,int j,double x);
   double get_elem(int i,int j) const;
   const MDO_vect inverse(const MDO_vect &vv);
   const MDO_matrix inverse();
   const MDO_matrix transpose();
   void diag(double aa,int n);
private:
   void clear();
   void ludcmp(int *indx,double *d);
   void lubksb(int *indx,MDO_vect &b);
   double **m;
   int size;
};

class Mdo : public Problem {

private:

  int MDA ( double   inputs[10] ,
	    double   Str   [5 ] ,
	    double & Apg        ,
	    double & SFC        ,
	    double & ESF        ,
	    double & Tb         ,
	    double & TbUA       ,
	    double & Temp       ,
	    double & Range      ,
	    double   eps        ,
	    int      max_it       ) const;

  void Struc ( double   inputs[10] ,
	       double   We         ,
	       double   L          ,
	       double & Wt         ,
	       double & Wf         ,
	       double & theta      ,
	       double   Str   [5 ]   ) const;

  void Aero ( double   inputs [10] ,
	      double   Wt          ,
	      double   theta       ,
	      double   ESF         ,
	      double & L           ,
	      double & D           ,
	      double & LD          ,
	      double & Apg           ) const;

  void Prop ( double   inputs [10] ,
	      double   D           ,
	      double & SFC         ,
	      double & We          ,
	      double & ESF         ,
	      double & Tb          ,
	      double & TbUA        ,
	      double & Temp          ) const;

  double pf ( double S      [] ,
	      double S_new  [] ,
	      double flag   [] ,
	      double S_bound[] ,
	      int    dim         ) const;

public:

  Mdo ( void );

  virtual ~Mdo ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
