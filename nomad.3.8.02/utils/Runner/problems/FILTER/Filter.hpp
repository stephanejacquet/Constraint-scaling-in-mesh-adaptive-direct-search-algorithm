#ifndef __FILTER__
#define __FILTER__

#include "../../Problem.hpp"

class Filter : public Problem {

public:

  Filter ( void );

  virtual ~Filter ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
