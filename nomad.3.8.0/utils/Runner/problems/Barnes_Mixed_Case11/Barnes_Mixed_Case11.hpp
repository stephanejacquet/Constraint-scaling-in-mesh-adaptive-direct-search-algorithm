#ifndef __BARNESMIXEDCASE11__
#define __BARNESMIXEDCASE11__

#include "../../Problem.hpp"

class Barnes_Mixed_Case11: public Problem {

public:

  Barnes_Mixed_Case11 ( void );

  virtual ~Barnes_Mixed_Case11 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
