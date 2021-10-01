#ifndef __ROSENBROCK__
#define __ROSENBROCK__

#include "../../Problem.hpp"

class Rosenbrock : public Problem {

public:

  Rosenbrock ( void );

  virtual ~Rosenbrock ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
