#ifndef __RASTRIGINMIXEDCASE11__
#define __RASTRIGINMIXEDCASE11__

#include "../../Problem.hpp"

class Rastrigin_Mixed_Case11: public Problem {

public:

  Rastrigin_Mixed_Case11 ( void );

  virtual ~Rastrigin_Mixed_Case11 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
