#ifndef __RASTRIGIN__
#define __RASTRIGIN__

#include "../../Problem.hpp"

class Rastrigin : public Problem {

public:

  Rastrigin ( void );

  virtual ~Rastrigin ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
