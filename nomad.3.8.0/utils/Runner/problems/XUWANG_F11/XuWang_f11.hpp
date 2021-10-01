#ifndef __XUWANG_F11__
#define __XUWANG_F11__

#include "../../Problem.hpp"

class XuWang_f11 : public Problem {

public:

  XuWang_f11 ( void );

  virtual ~XuWang_f11 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
