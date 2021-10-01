#ifndef __MAD6__
#define __MAD6__

#include "../../Problem.hpp"

class Mad6 : public Problem {

public:

  Mad6 ( void );

  virtual ~Mad6 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
