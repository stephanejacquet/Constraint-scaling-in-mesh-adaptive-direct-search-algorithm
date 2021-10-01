# Constraint-scaling-in-mesh-adaptive-direct-search-algorithm

This project shows the 3 version of MADS used in this article: Scaling of the output in Mesh Adaptive Direct Search, Pacific Journal of Optimization, Volume 16, Issue 4, 2020, Pages 595-610.  
It offers different ways to scale the outputs of blackboxes: first violation, median violation, maximum violation.
nomad 3.8.0 is the default version of nomad 3.8.0
nomad 3.8.01 offers scales only the output for the constraints violation function (h in the article)
nomad 3.8.01 offers a scale both for the constraints violation function, but also its surrogate.

The majors differences between these version are in Cache.cpp and Evaluator_Control.cpp, especially since h needs to be recalculated regularly using the function recalculate_h. 
