#ifndef __POLAK2__
#define __POLAK2__

#include "../../Problem.hpp"

class Polak2 : public Problem {

public:

  Polak2 ( void );

  virtual ~Polak2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
