#ifndef __VARDIM__
#define __VARDIM__

#include "../../Problem.hpp"

class Vardim : public Problem {

public:

  Vardim ( int n );

  virtual ~Vardim ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
