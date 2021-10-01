#ifndef __HS114__
#define __HS114__

#include "../../Problem.hpp"

class Hs114 : public Problem {

public:

  Hs114 ( void );

  virtual ~Hs114 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
