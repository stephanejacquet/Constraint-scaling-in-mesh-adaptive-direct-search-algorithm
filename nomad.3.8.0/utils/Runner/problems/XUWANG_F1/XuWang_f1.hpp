#ifndef __XUWANG_F1__
#define __XUWANG_F1__

#include "../../Problem.hpp"

class XuWang_f1 : public Problem {

public:

  XuWang_f1 ( void );

  virtual ~XuWang_f1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
