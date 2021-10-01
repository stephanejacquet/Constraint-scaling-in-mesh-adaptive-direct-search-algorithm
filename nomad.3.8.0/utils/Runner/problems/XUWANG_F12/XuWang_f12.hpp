#ifndef __XUWANG_F12__
#define __XUWANG_F12__

#include "../../Problem.hpp"

class XuWang_f12 : public Problem {

public:

  XuWang_f12 ( void );

  virtual ~XuWang_f12 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
