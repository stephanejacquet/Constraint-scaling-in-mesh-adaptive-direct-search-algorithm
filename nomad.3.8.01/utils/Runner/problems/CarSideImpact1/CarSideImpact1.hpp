#ifndef __CARSIDEIMPACT1__
#define __CARSIDEIMPACT1__

#include "../../Problem.hpp"

class CarSideImpact1: public Problem {

public:

  CarSideImpact1 ( void );

  virtual ~CarSideImpact1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
