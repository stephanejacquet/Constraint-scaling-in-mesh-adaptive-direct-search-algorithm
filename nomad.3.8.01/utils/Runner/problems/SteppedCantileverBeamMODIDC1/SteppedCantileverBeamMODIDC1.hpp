#ifndef __STEPPEDCANTILEVERBEAMMODIDC1__
#define __STEPPEDCANTILEVERBEAMMODIDC1__

#include "../../Problem.hpp"

class SteppedCantileverBeamMODIDC1: public Problem {

public:

  SteppedCantileverBeamMODIDC1 ( void );

  virtual ~SteppedCantileverBeamMODIDC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_SteppedCantileverBeamMODIDC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_SteppedCantileverBeamMODIDC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_SteppedCantileverBeamMODIDC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
