#ifndef __G07MIXEDCASE41__
#define __G07MIXEDCASE41__

#include "../../Problem.hpp"

class G07_Mixed_Case41: public Problem {

public:

  G07_Mixed_Case41 ( void );

  virtual ~G07_Mixed_Case41 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_G07_Mixed_Case41 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_G07_Mixed_Case41 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_G07_Mixed_Case41 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};

#endif
