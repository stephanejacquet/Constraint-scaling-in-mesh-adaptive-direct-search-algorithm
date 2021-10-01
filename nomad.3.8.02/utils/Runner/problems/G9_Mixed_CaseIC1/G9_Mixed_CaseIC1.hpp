#ifndef __G9MIXEDCASEIC1__
#define __G9MIXEDCASEIC1__

#include "../../Problem.hpp"

class G9_Mixed_CaseIC1: public Problem {

public:

  G9_Mixed_CaseIC1 ( void );

  virtual ~G9_Mixed_CaseIC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_G9_Mixed_CaseIC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_G9_Mixed_CaseIC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_G9_Mixed_CaseIC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};




#endif
