#ifndef __MXHILB__
#define __MXHILB__

#include "../../Problem.hpp"

class MxHilb : public Problem {

public:

  MxHilb ( void );

  virtual ~MxHilb ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
