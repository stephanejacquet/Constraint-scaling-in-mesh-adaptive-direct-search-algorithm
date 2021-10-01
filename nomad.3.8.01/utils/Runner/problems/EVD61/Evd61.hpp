#ifndef __EVD61__
#define __EVD61__

#include "../../Problem.hpp"

class Evd61 : public Problem {

public:

  Evd61 ( void );

  virtual ~Evd61 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
