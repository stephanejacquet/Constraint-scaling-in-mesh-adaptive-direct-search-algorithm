#ifndef __STEPPEDCANTILEVERBEAMMOD1__
#define __STEPPEDCANTILEVERBEAMMOD1__

#include "../../Problem.hpp"

class SteppedCantileverBeamMOD1: public Problem {

public:

  SteppedCantileverBeamMOD1 ( void );

  virtual ~SteppedCantileverBeamMOD1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
