/*-------------------------------------------------------------------------------------*/
/*  NOMAD - Nonlinear Optimization by Mesh Adaptive Direct search - version 3.8.0      */
/*                                                                                     */
/*                                                                                     */
/*  NOMAD - version 3.8.0 has been created by                                          */
/*                 Charles Audet        - Ecole Polytechnique de Montreal              */
/*                 Sebastien Le Digabel - Ecole Polytechnique de Montreal              */
/*                 Christophe Tribes    - Ecole Polytechnique de Montreal              */
/*                                                                                     */
/*  The copyright of NOMAD - version 3.8.0 is owned by                                 */
/*                 Sebastien Le Digabel - Ecole Polytechnique de Montreal              */
/*                 Christophe Tribes    - Ecole Polytechnique de Montreal              */
/*                                                                                     */
/*  NOMAD v3 has been funded by AFOSR and Exxon Mobil.                                 */
/*                                                                                     */
/*  NOMAD v3 is a new version of NOMAD v1 and v2. NOMAD v1 and v2 were created and     */
/*  developed by Mark Abramson, Charles Audet, Gilles Couture and John E. Dennis Jr.,  */
/*  and were funded by AFOSR and Exxon Mobil.                                          */
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
 \file   Sgtelib_Model_Manager.hpp
 \brief  Handle the sgtelib_model model for the search and the eval_sort.
 \author Bastien Talgorn
 \date   2013-04-25
 \see    Sgtelib_Model_Search.cpp
 */

#ifndef __SGTELIB_MODEL_MANAGER__
#define __SGTELIB_MODEL_MANAGER__

#include "Parameters.hpp"
#include "Evaluator_Control.hpp"

#include "sgtelib.hpp"



namespace NOMAD {
    
    
    // This class handle the Sgtelib_Model, the calls to this model, the predictions
    // and the various parameters (formulation, lambda, etc...)
    class Sgtelib_Model_Manager {
        
    private:
        
        // General data
        const NOMAD::Parameters  & _p;  ///< Parameters.
        NOMAD::Evaluator_Control * _ev_control; ///< Evaluator control.
        
        // MODEL
        SGTELIB::TrainingSet * _trainingset;
        SGTELIB::Surrogate   * _model;
        
        // the number of models
        int _nb_models;
        
        int _highest_tag; // Plus haut tag actuellement contenu dans la cache
        bool _ready;
        bool _found_feasible; // True if a feasible point has been found
        NOMAD::Point _model_lb; // lower bound
        NOMAD::Point _model_ub; // upper bound
        
        // information sur la search
        NOMAD::Double _search_pfi_max;
        NOMAD::Double _search_efi_max;
        NOMAD::Double _search_obj_min;
        const NOMAD::Cache  & _cache ;
        //void _initialisation(void);
        void _set_model_bounds (SGTELIB::Matrix * X);
        
    public:
        
        // Constructor
        Sgtelib_Model_Manager ( NOMAD::Parameters & p , NOMAD::Evaluator_Control * ev_control,const NOMAD::Cache & cache  );
        
        // Destructor.
        ~Sgtelib_Model_Manager ( void ) { reset(); };
        
        bool is_ready(void);
        void update(void);
        void reset(void);
        void info(void);
        
        // search_stats
        void reset_search_stats(void);
        void write_search_stats(void) const;
        void update_search_stats( const NOMAD::Double & pfi,
                                 const NOMAD::Double & efi,
                                 const NOMAD::Double & obj);
        
        // Compute stats measurements from sgtelib_model outputs
        
        void eval_h (  const NOMAD::Point  & bbo    ,
                     NOMAD::Double       & h      ) const;
        
        bool eval_x (  NOMAD::Eval_Point   * x          ,
                     const NOMAD::Double & h_max      ,
                     bool                & count_eval );
        
        const sgtelib_model_formulation_type get_formulation( void );
        
        void check_hf ( NOMAD::Eval_Point   * x );
        
        SGTELIB::Surrogate * get_model(void){return _model;}
        
        NOMAD::Evaluator_Control * get_evaluator_control(void){ return _ev_control;}
        
        NOMAD::Point get_extended_lb(void);
        NOMAD::Point get_extended_ub(void);
        
        NOMAD::Double get_f_min (void);
        
        
        
    };// End of class
}// End of Namespace

#endif
// #endif

