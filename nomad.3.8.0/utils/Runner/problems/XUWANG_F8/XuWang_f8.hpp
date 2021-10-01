#ifndef __XUWANG_F8__
#define __XUWANG_F8__

#include "../../Problem.hpp"

class XuWang_f8 : public Problem {

public:

  XuWang_f8 ( void );

  virtual ~XuWang_f8 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
