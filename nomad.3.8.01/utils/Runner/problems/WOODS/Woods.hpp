#ifndef __WOODS__
#define __WOODS__

#include "../../Problem.hpp"

class Woods : public Problem {

public:

  Woods ( int n );

  virtual ~Woods ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
