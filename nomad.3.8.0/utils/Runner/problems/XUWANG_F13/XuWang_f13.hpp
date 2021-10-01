#ifndef __XUWANG_F13__
#define __XUWANG_F13__

#include "../../Problem.hpp"

class XuWang_f13 : public Problem {

public:

  XuWang_f13 ( void );

  virtual ~XuWang_f13 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

#endif
