#ifndef __REINFORCEDCONCRETEBEAMMOD1__
#define __REINFORCEDCONCRETEBEAMMOD1__

#include "../../Problem.hpp"

class ReinforcedConcreteBeamMOD1: public Problem {

public:

  ReinforcedConcreteBeamMOD1 ( void );

  virtual ~ReinforcedConcreteBeamMOD1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
