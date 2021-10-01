#ifndef __PENALTY1__
#define __PENALTY1__

#include "../../Problem.hpp"

class Penalty1 : public Problem {

public:

  Penalty1 ( int n );

  virtual ~Penalty1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
