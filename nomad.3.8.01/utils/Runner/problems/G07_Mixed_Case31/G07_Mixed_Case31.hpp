#ifndef __G07MIXEDCASE31__
#define __G07MIXEDCASE31__

#include "../../Problem.hpp"

class G07_Mixed_Case31: public Problem {

public:

  G07_Mixed_Case31 ( void );

  virtual ~G07_Mixed_Case31 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
