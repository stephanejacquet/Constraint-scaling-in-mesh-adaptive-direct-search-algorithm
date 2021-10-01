#ifndef __XUWANG_F5__
#define __XUWANG_F5__

#include "../../Problem.hpp"

class XuWang_f5 : public Problem {

public:

  XuWang_f5 ( void );

  virtual ~XuWang_f5 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
