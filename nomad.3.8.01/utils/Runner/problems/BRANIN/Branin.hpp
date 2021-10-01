#ifndef __BRANIN__
#define __BRANIN__

#include "../../Problem.hpp"

class Branin : public Problem {

public:

  Branin ( void );

  virtual ~Branin ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
