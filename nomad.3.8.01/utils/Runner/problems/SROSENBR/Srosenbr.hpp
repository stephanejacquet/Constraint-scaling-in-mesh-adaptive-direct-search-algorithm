#ifndef __SROSENBR__
#define __SROSENBR__

#include "../../Problem.hpp"

class Srosenbr : public Problem {

public:

  Srosenbr ( int n );

  virtual ~Srosenbr ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
