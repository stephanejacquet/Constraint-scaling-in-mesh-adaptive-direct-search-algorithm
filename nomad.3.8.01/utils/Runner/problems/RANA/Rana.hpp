#ifndef __RANA__
#define __RANA__

#include "../../Problem.hpp"

class Rana : public Problem {

public:

  Rana ( void );

  virtual ~Rana ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
