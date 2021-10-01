#ifndef __HS78__
#define __HS78__

#include "../../Problem.hpp"

class Hs78 : public Problem {

public:

  Hs78 ( void );

  virtual ~Hs78 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
