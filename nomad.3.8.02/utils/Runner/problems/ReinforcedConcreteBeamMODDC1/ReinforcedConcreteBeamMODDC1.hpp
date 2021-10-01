#ifndef __REINFORCEDCONCRETEBEAMMODDC1__
#define __REINFORCEDCONCRETEBEAMMODDC1__

#include "../../Problem.hpp"

class ReinforcedConcreteBeamMODDC1: public Problem {

public:

  ReinforcedConcreteBeamMODDC1 ( void );

  virtual ~ReinforcedConcreteBeamMODDC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_ReinforcedConcreteBeamMODDC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_ReinforcedConcreteBeamMODDC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_ReinforcedConcreteBeamMODDC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
