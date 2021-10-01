#ifndef __MYSTERYMIXEDCASE11__
#define __MYSTERYMIXEDCASE11__

#include "../../Problem.hpp"

class Mystery_Mixed_Case11: public Problem {

public:

  Mystery_Mixed_Case11 ( void );

  virtual ~Mystery_Mixed_Case11 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
