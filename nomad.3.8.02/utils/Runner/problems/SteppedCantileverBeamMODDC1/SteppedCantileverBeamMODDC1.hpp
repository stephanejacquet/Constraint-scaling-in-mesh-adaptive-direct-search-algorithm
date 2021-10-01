#ifndef __STEPPEDCANTILEVERBEAMMODDC1__
#define __STEPPEDCANTILEVERBEAMMODDC1__

#include "../../Problem.hpp"

class SteppedCantileverBeamMODDC1: public Problem {

public:

  SteppedCantileverBeamMODDC1 ( void );

  virtual ~SteppedCantileverBeamMODDC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_SteppedCantileverBeamMODDC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_SteppedCantileverBeamMODDC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_SteppedCantileverBeamMODDC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
