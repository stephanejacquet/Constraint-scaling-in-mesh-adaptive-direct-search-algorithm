#ifndef __SNAKE__
#define __SNAKE__

#include "../../Problem.hpp"

class Snake : public Problem {

public:

  Snake ( void );

  virtual ~Snake ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
