#ifndef __SPRINGMODMIXEDCASE1__
#define __SPRINGMODMIXEDCASE1__

#include "../../Problem.hpp"

class SpringMOD_Mixed_Case1: public Problem {

public:

  SpringMOD_Mixed_Case1 ( void );

  virtual ~SpringMOD_Mixed_Case1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
