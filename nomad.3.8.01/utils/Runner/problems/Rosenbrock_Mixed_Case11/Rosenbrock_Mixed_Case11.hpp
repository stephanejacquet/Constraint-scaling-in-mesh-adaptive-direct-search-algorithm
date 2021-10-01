#ifndef __ROSENBROCKMIXEDCASE11__
#define __ROSENBROCKMIXEDCASE11__

#include "../../Problem.hpp"

class Rosenbrock_Mixed_Case11: public Problem {

public:

  Rosenbrock_Mixed_Case11 ( void );

  virtual ~Rosenbrock_Mixed_Case11 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
