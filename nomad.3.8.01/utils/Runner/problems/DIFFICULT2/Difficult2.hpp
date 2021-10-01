#ifndef __DIFFICULT2__
#define __DIFFICULT2__

#include "../../Problem.hpp"

class Difficult2 : public Problem {

public:

  Difficult2 ( void );

  virtual ~Difficult2 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
