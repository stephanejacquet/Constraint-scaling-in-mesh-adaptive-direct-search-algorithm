#ifndef __PRESSUREVESSELMIXEDCASE1__
#define __PRESSUREVESSELMIXEDCASE1__

#include "../../Problem.hpp"

class PressureVessel_Mixed_Case1: public Problem {

public:

  PressureVessel_Mixed_Case1 ( void );

  virtual ~PressureVessel_Mixed_Case1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
