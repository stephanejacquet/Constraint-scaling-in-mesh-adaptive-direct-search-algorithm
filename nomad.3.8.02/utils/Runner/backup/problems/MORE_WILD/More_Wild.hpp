#ifndef __MORE_WILD__
#define __MORE_WILD__

#include "../../Problem.hpp"

enum MW_pb_type {
  MW_SMOOTH ,  // smooth
  MW_NONDIFF,  // nonsmooth
  MW_WILD3  ,  // deterministic noise
  MW_NOISY3    // random noise
};

/*-------------*/
/*  constants  */
/*-------------*/
const double MW_PI       = 3.141592653589793;
const double MW_SQRT5    = sqrt(5.0);
const double MW_SQRT10   = sqrt(10.0);
const double MW_C13      = 13.0;
const double MW_C14      = 14.0;
const double MW_C29      = 29.0;
const double MW_C45      = 45.0;
const double MW_DINT_MAX = INT_MAX;

const double MW_Y1[] = { 0.14 , 0.18 , 0.22 , 0.25 , 0.29 ,
			 0.32 , 0.35 , 0.39 , 0.37 , 0.58 ,
			 0.73 , 0.96 , 1.34 , 2.10 , 4.39   };

const double MW_Y2[] = { 0.1957 , 0.1947 , 0.1735 , 0.1600 ,
			 0.0844 , 0.0627 , 0.0456 , 0.0342 ,
			 0.0323 , 0.0235 , 0.0246 };

const double MW_Y3[] = { 34780 , 28610 , 23650 , 19630 ,
			 16370 , 13720 , 11540 ,  9744 ,
			 8261 ,  7030 ,  6005 ,  5147 ,
			 4427 ,  3820 ,  3307 ,  2872   };

const double MW_Y4[] = { 0.844 , 0.908 , 0.932 , 0.936 , 0.925 ,
			 0.908 , 0.881 , 0.850 , 0.818 , 0.784 ,
			 0.751 , 0.718 , 0.685 , 0.658 , 0.628 ,
			 0.603 , 0.580 , 0.558 , 0.538 , 0.522 ,
			 0.506 , 0.490 , 0.478 , 0.467 , 0.457 ,
			 0.448 , 0.438 , 0.431 , 0.424 , 0.420 ,
			 0.414 , 0.411 , 0.406 };

const double MW_Y5[] = { 1.366 , 1.191 , 1.112 , 1.013 , 0.991 ,
			 0.885 , 0.831 , 0.847 , 0.786 , 0.725 ,
			 0.746 , 0.679 , 0.608 , 0.655 , 0.616 ,
			 0.606 , 0.602 , 0.626 , 0.651 , 0.724 ,
			 0.649 , 0.649 , 0.694 , 0.644 , 0.624 ,
			 0.661 , 0.612 , 0.558 , 0.533 , 0.495 ,
			 0.500 , 0.423 , 0.395 , 0.375 , 0.372 ,
			 0.391 , 0.396 , 0.405 , 0.428 , 0.429 ,
			 0.523 , 0.562 , 0.607 , 0.653 , 0.672 ,
			 0.708 , 0.633 , 0.668 , 0.645 , 0.632 ,
			 0.591 , 0.559 , 0.597 , 0.625 , 0.739 ,
			 0.710 , 0.729 , 0.720 , 0.636 , 0.581 ,
			 0.428 , 0.292 , 0.162 , 0.098 , 0.054   };

const double MW_V[] = { 4 , 2 , 1 , 0.5 , 0.25 , 0.167 , 0.125 ,
			0.1 , 0.0833 , 0.0714 , 0.0625 };

class More_Wild : public Problem {

private:

  MW_pb_type _MW_pb_type;
  int        _MW_nprob;
  int        _MW_m;

  // get problem dimension:
  int get_pb_n ( int n_instance ) const;

  // get problem name:
  void get_pb_name ( int           nprob       ,
		     std::string & name        ,
		     std::string & description   ) const;

  // get problem id:
  std::string get_pb_id ( int n_instance , MW_pb_type pb_type ) const;

  // get instance data:
  void get_instance_data ( int               instance ,
			   MW_pb_type        pbtype   ,
			   int             & nprob    ,
			   int             & n        ,
			   int             & m        ,
			   bool            & ns       ,
			   std::string     & name       ) const;

  // dfoxs function:
  void dfoxs ( int      n      ,
	       int      nprob  ,
	       double   factor ,
	       double * x        ) const;

  // dfovec function:
  bool dfovec ( int            m     ,
		int            n     ,
		const double * x     ,
		int            nprob ,
		double       * fvec    ) const;
public:

  More_Wild ( int n_instance , MW_pb_type pb_type );

  virtual ~More_Wild ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
