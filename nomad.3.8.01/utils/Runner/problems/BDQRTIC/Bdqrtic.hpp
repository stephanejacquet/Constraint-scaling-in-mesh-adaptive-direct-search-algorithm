#ifndef __BDQRTIC__
#define __BDQRTIC__

#include "../../Problem.hpp"

class Bdqrtic : public Problem {

public:

  Bdqrtic ( int n );

  virtual ~Bdqrtic ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
