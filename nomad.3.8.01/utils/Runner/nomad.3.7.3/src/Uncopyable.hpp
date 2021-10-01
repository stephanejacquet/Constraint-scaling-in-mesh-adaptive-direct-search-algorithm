/*-------------------------------------------------------------------------------------*/
/*  NOMAD - Nonlinear Optimization by Mesh Adaptive Direct search - version 3.7.3      */
/*                                                                                     */
/*                                                                                     */
/*  NOMAD - version 3.7.3 has been created by                                          */
/*                 Charles Audet        - Ecole Polytechnique de Montreal              */
/*                 Sebastien Le Digabel - Ecole Polytechnique de Montreal              */
/*                 Christophe Tribes    - Ecole Polytechnique de Montreal              */
/*                                                                                     */
/*  The copyright of NOMAD - version 3.7.3 is owned by                                 */
/*                 Sebastien Le Digabel - Ecole Polytechnique de Montreal              */
/*                 Christophe Tribes    - Ecole Polytechnique de Montreal              */
/*                                                                                     */
/*  NOMAD v3 has been funded by AFOSR and Exxon Mobil.                                 */
/*                                                                                     */
/*  NOMAD v3 is a new version of Nomad v1 and v2. Nomad v1 and v2 were created and     */
/*  developed by Mark A. Abramson from The Boeing Company, Charles Audet and           */
/*  Gilles Couture from Ecole Polytechnique de Montreal, and John E. Dennis Jr. from   */
/*  Rice University, and were funded by AFOSR and Exxon Mobil.                         */
/*                                                                                     */
/*                                                                                     */
/*  Contact information:                                                               */
/*    Ecole Polytechnique de Montreal - GERAD                                          */
/*    C.P. 6079, Succ. Centre-ville, Montreal (Quebec) H3C 3A7 Canada                  */
/*    e-mail: nomad@gerad.ca                                                           */
/*    phone : 1-514-340-6053 #6928                                                     */
/*    fax   : 1-514-340-5665                                                           */
/*                                                                                     */
/*  This program is free software: you can redistribute it and/or modify it under the  */
/*  terms of the GNU Lesser General Public License as published by the Free Software   */
/*  Foundation, either version 3 of the License, or (at your option) any later         */
/*  version.                                                                           */
/*                                                                                     */
/*  This program is distributed in the hope that it will be useful, but WITHOUT ANY    */
/*  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A    */
/*  PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.   */
/*                                                                                     */
/*  You should have received a copy of the GNU Lesser General Public License along     */
/*  with this program. If not, see <http://www.gnu.org/licenses/>.                     */
/*                                                                                     */
/*  You can find information on the NOMAD software at www.gerad.ca/nomad               */
/*-------------------------------------------------------------------------------------*/
/**
  \file   Uncopyable.hpp
  \brief  Base class for uncopyable classes (headers)
  \author Sebastien Le Digabel
  \date   2010-04-02
*/
#ifndef __UNCOPYABLE__
#define __UNCOPYABLE__

namespace NOMAD {

  /// Uncopyable class.
  /**
     Base class for uncopyable classes
     (see Scott Meyer's Effective C++, 3rd ed., item #6).
  */
  class Uncopyable {

  protected:

    /// Constructor.
    explicit Uncopyable  ( void ) {}

    /// Destructor.
    virtual ~Uncopyable ( void ) {}

  private:
    
    /// Undefined copy constructor.
    Uncopyable ( const Uncopyable & );

    /// Undefined affectation operator.
    Uncopyable & operator = ( const Uncopyable & );
  };
}

#endif
