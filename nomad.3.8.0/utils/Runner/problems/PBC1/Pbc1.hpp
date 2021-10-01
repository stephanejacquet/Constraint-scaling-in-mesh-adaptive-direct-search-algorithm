#ifndef __PBC1__
#define __PBC1__

#include "../../Problem.hpp"

class Pbc1 : public Problem {

public:

  Pbc1 ( void );

  virtual ~Pbc1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
