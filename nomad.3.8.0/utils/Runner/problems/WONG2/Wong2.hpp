#ifndef __WONG2__
#define __WONG2__

#include "../../Problem.hpp"

class Wong2 : public Problem {

public:

  Wong2 ( void );

  virtual ~Wong2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
