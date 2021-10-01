#ifndef __WATSON12__
#define __WATSON12__

#include "../../Problem.hpp"

class Watson12 : public Problem {

public:

  Watson12 ( void );

  virtual ~Watson12 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
