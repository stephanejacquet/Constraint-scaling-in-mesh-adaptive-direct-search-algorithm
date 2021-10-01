#ifndef __WONG1__
#define __WONG1__

#include "../../Problem.hpp"

class Wong1 : public Problem {

public:

  Wong1 ( void );

  virtual ~Wong1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
