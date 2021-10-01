#ifndef __OSBORNE2__
#define __OSBORNE2__

#include "../../Problem.hpp"

class Osborne2 : public Problem {

public:

  Osborne2 ( void );

  virtual ~Osborne2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
