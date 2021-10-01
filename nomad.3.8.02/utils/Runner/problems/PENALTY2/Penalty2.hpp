#ifndef __PENALTY2__
#define __PENALTY2__

#include "../../Problem.hpp"

class Penalty2 : public Problem {

public:

  Penalty2 ( int n );

  virtual ~Penalty2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
