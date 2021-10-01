#ifndef __G9MIXEDCASE1__
#define __G9MIXEDCASE1__

#include "../../Problem.hpp"

class G9_Mixed_Case1: public Problem {

public:

  G9_Mixed_Case1 ( void );

  virtual ~G9_Mixed_Case1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
