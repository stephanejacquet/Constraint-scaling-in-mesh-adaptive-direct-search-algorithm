#ifndef __SPEEDREDUCERMODMIXEDCASE1__
#define __SPEEDREDUCERMODMIXEDCASE1__

#include "../../Problem.hpp"

class SpeedReducerMOD_Mixed_Case1: public Problem {

public:

  SpeedReducerMOD_Mixed_Case1 ( void );

  virtual ~SpeedReducerMOD_Mixed_Case1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
