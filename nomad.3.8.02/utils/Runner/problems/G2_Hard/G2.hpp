#ifndef __G2__
#define __G2__

#include "../../Problem.hpp"

class G2 : public Problem {

public:

  G2 ( int n );

  virtual ~G2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
