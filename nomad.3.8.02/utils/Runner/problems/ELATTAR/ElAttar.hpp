#ifndef __ELATTAR__
#define __ELATTAR__

#include "../../Problem.hpp"

class ElAttar : public Problem {

public:

  ElAttar ( void );

  virtual ~ElAttar ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
