#ifndef __B500__
#define __B500__

#include "../../Problem.hpp"

const double B500_SQRT_A = pow(10.0,-2.5);
const double B500_E1     = exp(-0.1);

const double B500_H  = 0.0625;
const double B500_H2 = 0.00390625;

const int B500_J[15][6] = {
  { 2 } ,
  { 1  , 3 } ,
  { 1  , 2  , 4 } ,
  { 1  , 2  , 3  , 5 } ,
  { 1  , 2  , 3  , 4  , 6 } ,
  { 1  , 2  , 3  , 4  , 5  , 7 } ,
  { 2  , 3  , 4  , 5  , 6  , 8 } ,
  { 3  , 4  , 5  , 6  , 7  , 9 } ,
  { 4  , 5  , 6  , 7  , 8  , 10 } ,
  { 5  , 6  , 7  , 8  , 9  , 11 } ,
  { 6  , 7  , 8  , 9  , 10 , 12 } ,
  { 7  , 8  , 9  , 10 , 11 , 13 } ,
  { 8  , 9  , 10 , 11 , 12 , 14 } ,
  { 9  , 10 , 11 , 12 , 13 , 15 } ,
  { 10 , 11 , 12 , 13 , 14 }
};

  const int B500_L [15]
  = { 1 , 2 , 3 , 4 , 5 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 6 , 5 };

class B500 : public Problem {

private:

  double f_penalty           ( double x[60] ) const;
  double f_trigo             ( double x[60] ) const;
  double f_brown             ( double x[60] ) const;
  double f_broyden_banden    ( double x[60] ) const;
  double f_broyden_tridiag   ( double x[60] ) const;
  double f_discrete_boundary ( double x[60] ) const;

public:

  B500 ( void );

  virtual ~B500 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
