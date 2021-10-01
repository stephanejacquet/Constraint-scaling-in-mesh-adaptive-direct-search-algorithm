#ifndef __RADAR7__
#define __RADAR7__

#include "../../Problem.hpp"

class Radar7 : public Problem {

public:

  Radar7 ( void );

  virtual ~Radar7 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
