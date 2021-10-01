#ifndef __SHOR__
#define __SHOR__

#include "../../Problem.hpp"

class Shor : public Problem {

public:

  Shor ( void );

  virtual ~Shor ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
