#ifndef __MYSTERYMIXEDCASE21__
#define __MYSTERYMIXEDCASE21__

#include "../../Problem.hpp"

class Mystery_Mixed_Case21: public Problem {

public:

  Mystery_Mixed_Case21 ( void );

  virtual ~Mystery_Mixed_Case21 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
