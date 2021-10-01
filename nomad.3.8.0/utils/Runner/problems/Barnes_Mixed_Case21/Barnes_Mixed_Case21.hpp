#ifndef __BARNESMIXEDCASE21__
#define __BARNESMIXEDCASE21__

#include "../../Problem.hpp"

class Barnes_Mixed_Case21: public Problem {

public:

  Barnes_Mixed_Case21 ( void );

  virtual ~Barnes_Mixed_Case21 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
