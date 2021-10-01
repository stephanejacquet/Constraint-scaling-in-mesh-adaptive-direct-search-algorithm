#ifndef __BIGGS6__
#define __BIGGS6__

#include "../../Problem.hpp"

class Biggs6 : public Problem {

public:

  Biggs6 ( void );

  virtual ~Biggs6 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
