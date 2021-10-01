#ifndef __GRIEWANK__
#define __GRIEWANK__

#include "../../Problem.hpp"

class Griewank : public Problem {

public:

  Griewank ( void );

  virtual ~Griewank ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
