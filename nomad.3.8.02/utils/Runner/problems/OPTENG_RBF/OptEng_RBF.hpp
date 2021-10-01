#ifndef __OPTENG_RBF__
#define __OPTENG_RBF__

#include "../../Problem.hpp"

class OptEng_RBF : public Problem {

public:

  OptEng_RBF ( void );

  virtual ~OptEng_RBF ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
