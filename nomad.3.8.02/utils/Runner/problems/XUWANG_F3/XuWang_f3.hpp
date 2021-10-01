#ifndef __XUWANG_F3__
#define __XUWANG_F3__

#include "../../Problem.hpp"

class XuWang_f3 : public Problem {

public:

  XuWang_f3 ( void );

  virtual ~XuWang_f3 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
