#ifndef __DISK__
#define __DISK__

#include "../../Problem.hpp"

class Disk : public Problem {

public:

  Disk ( int n );

  virtual ~Disk ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
