#ifndef __TREFETHEN__
#define __TREFETHEN__

#include "../../Problem.hpp"

class Trefethen : public Problem {

public:

  Trefethen ( void );

  virtual ~Trefethen ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
