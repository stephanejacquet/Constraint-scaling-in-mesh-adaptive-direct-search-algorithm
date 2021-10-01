#ifndef __GOFFIN__
#define __GOFFIN__

#include "../../Problem.hpp"

class Goffin : public Problem {

public:

  Goffin ( void );

  virtual ~Goffin ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
