#ifndef __BROWNAL__
#define __BROWNAL__

#include "../../Problem.hpp"

class Brownal : public Problem {

public:

  Brownal ( int n );

  virtual ~Brownal ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
