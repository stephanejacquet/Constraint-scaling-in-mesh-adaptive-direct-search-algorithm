#ifndef __REINFORCEDCONCRETEBEAMMODIDC1__
#define __REINFORCEDCONCRETEBEAMMODIDC1__

#include "../../Problem.hpp"

class ReinforcedConcreteBeamMODIDC1: public Problem {

public:

  ReinforcedConcreteBeamMODIDC1 ( void );

  virtual ~ReinforcedConcreteBeamMODIDC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_ReinforcedConcreteBeamMODIDC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_ReinforcedConcreteBeamMODIDC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_ReinforcedConcreteBeamMODIDC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
