#ifndef __BARNESMIXEDCASE31__
#define __BARNESMIXEDCASE31__

#include "../../Problem.hpp"

class Barnes_Mixed_Case31: public Problem
{

public:
    
    Barnes_Mixed_Case31 ( void );
    
    virtual ~Barnes_Mixed_Case31 ( void ) {}
    
    virtual bool eval_x ( NOMAD::Eval_Point & x          ,
                         bool               & count_eval   ) const;
    
};

/*--------------------------------------------------*/
/*  user class to define categorical neighborhoods  */
/*--------------------------------------------------*/
class EP_Barnes_Mixed_Case31 : public NOMAD::Extended_Poll
{
public:
    
    // constructor:
    EP_Barnes_Mixed_Case31 ( NOMAD::Parameters & p):
    Extended_Poll ( p    ) {}
    
    // destructor:
    virtual ~EP_Barnes_Mixed_Case31 ( void ) {}
    
    // construct the extended poll points:
    virtual void construct_extended_points ( const NOMAD::Eval_Point & );
    
};

#endif
