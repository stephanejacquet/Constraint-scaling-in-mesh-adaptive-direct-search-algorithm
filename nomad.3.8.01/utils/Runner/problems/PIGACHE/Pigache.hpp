#ifndef __PIGACHE__
#define __PIGACHE__

#include "../../Problem.hpp"

class Pigache : public Problem {
public:

  Pigache ( void );

  virtual ~Pigache ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
