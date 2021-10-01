#ifndef __REINFORCEDCONCRETEBEAMMODIC1__
#define __REINFORCEDCONCRETEBEAMMODIC1__

#include "../../Problem.hpp"

class ReinforcedConcreteBeamMODIC1: public Problem {

public:

  ReinforcedConcreteBeamMODIC1 ( void );

  virtual ~ReinforcedConcreteBeamMODIC1 ( void ) {}

  virtual bool eval_x ( NOMAD::Eval_Point & x          ,
 			bool              & count_eval   ) const;
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_ReinforcedConcreteBeamMODIC1 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_ReinforcedConcreteBeamMODIC1 ( NOMAD::Parameters & p):
    NOMAD::Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_ReinforcedConcreteBeamMODIC1 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};


#endif
