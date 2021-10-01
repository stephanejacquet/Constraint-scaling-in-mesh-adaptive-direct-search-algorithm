#ifndef __STEPPEDCANTILEVERBEAMMODIC1__
#define __STEPPEDCANTILEVERBEAMMODIC1__

#include "../../Problem.hpp"

class SteppedCantileverBeamMODIC1: public Problem {

public:

  SteppedCantileverBeamMODIC1 ( void );

  virtual ~SteppedCantileverBeamMODIC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_SteppedCantileverBeamMODIC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_SteppedCantileverBeamMODIC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_SteppedCantileverBeamMODIC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
