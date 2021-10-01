#ifndef __XUWANG_F7__
#define __XUWANG_F7__

#include "../../Problem.hpp"

class XuWang_f7 : public Problem {

public:

  XuWang_f7 ( int n );

  virtual ~XuWang_f7 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
