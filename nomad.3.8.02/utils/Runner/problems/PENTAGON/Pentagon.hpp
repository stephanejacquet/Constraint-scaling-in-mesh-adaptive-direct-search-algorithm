#ifndef __PENTAGON__
#define __PENTAGON__

#include "../../Problem.hpp"

class Pentagon : public Problem {

public:

  Pentagon ( void );

  virtual ~Pentagon ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
