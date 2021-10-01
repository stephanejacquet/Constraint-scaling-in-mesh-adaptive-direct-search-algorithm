#ifndef __XUWANG_F10__
#define __XUWANG_F10__

#include "../../Problem.hpp"

class XuWang_f10 : public Problem {

public:

  XuWang_f10 ( void );

  virtual ~XuWang_f10 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
