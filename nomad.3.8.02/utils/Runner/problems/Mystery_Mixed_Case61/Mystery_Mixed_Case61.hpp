#ifndef __MYSTERYMIXEDCASE61__
#define __MYSTERYMIXEDCASE61__

#include "../../Problem.hpp"

class Mystery_Mixed_Case61: public Problem {

public:

  Mystery_Mixed_Case61 ( void );

  virtual ~Mystery_Mixed_Case61 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};


#endif
