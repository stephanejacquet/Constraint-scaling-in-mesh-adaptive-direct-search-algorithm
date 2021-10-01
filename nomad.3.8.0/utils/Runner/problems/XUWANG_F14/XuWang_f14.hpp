#ifndef __XUWANG_F14__
#define __XUWANG_F14__

#include "../../Problem.hpp"

class XuWang_f14 : public Problem {

public:

  XuWang_f14 ( void );

  virtual ~XuWang_f14 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
