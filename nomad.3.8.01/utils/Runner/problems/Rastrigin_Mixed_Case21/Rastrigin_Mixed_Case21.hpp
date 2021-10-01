#ifndef __RASTRIGINMIXEDCASE21__
#define __RASTRIGINMIXEDCASE21__

#include "../../Problem.hpp"

class Rastrigin_Mixed_Case21: public Problem {

public:

  Rastrigin_Mixed_Case21 ( void );

  virtual ~Rastrigin_Mixed_Case21 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
