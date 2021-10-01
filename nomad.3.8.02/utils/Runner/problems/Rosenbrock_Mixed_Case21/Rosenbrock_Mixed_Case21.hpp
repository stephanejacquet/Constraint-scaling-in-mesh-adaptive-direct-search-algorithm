#ifndef __ROSENBROCKMIXEDCASE21__
#define __ROSENBROCKMIXEDCASE21__

#include "../../Problem.hpp"

class Rosenbrock_Mixed_Case21: public Problem {

public:

  Rosenbrock_Mixed_Case21 ( void );

  virtual ~Rosenbrock_Mixed_Case21 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
