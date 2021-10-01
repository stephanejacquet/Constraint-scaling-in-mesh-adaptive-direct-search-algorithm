#ifndef __POWELLSG__
#define __POWELLSG__

#include "../../Problem.hpp"

class Powellsg : public Problem {

public:

  Powellsg ( int n );

  virtual ~Powellsg ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
