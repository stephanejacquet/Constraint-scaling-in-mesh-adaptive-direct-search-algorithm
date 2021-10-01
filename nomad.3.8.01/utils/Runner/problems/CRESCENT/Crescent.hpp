#ifndef __CRESCENT__
#define __CRESCENT__

#include "../../Problem.hpp"

class Crescent : public Problem {

public:

  Crescent ( int n );

  virtual ~Crescent ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
