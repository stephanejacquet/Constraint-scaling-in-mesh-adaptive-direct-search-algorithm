#ifndef __TRIDIA__
#define __TRIDIA__

#include "../../Problem.hpp"

class Tridia : public Problem {

public:

  Tridia ( int n );

  virtual ~Tridia ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
