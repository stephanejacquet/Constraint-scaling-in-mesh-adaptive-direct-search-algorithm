#ifndef __L1HILB__
#define __L1HILB__

#include "../../Problem.hpp"

class L1Hilb : public Problem {

public:

  L1Hilb ( void );

  virtual ~L1Hilb ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
