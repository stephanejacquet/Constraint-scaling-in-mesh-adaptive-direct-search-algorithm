#ifndef __RHEOLOGY__
#define __RHEOLOGY__

#include "../../Problem.hpp"

const double GAMMA[13] =
  { 0.0137 ,
    0.0274 ,
    0.0434 ,
    0.0866 ,
    0.137  , 
    0.274  ,
    0.434  , 
    0.866  ,
    1.37   ,
    2.74   ,
    4.34   ,
    5.46   ,
    6.88     };

const double ETA[13] =
  { 3220 ,
    2190 ,
    1640 ,
    1050 ,
    766 ,
    490 ,
    348 ,
    223 ,
    163 ,
    104 ,
    76.7 ,
    68.1 ,
    58.2   };

class Rheology : public Problem {

public:

  Rheology ( void );

  virtual ~Rheology ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
