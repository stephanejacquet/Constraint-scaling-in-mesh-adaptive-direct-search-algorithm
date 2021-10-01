#ifndef __ARWHEAD__
#define __ARWHEAD__

#include "../../Problem.hpp"

class Arwhead : public Problem {

public:

  Arwhead ( int n );

  virtual ~Arwhead ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
