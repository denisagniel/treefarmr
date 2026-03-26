# Utility functions for Rashomon set operations

#' Get property from CFRashomon object (S7 or S3)
#' @keywords internal
get_rashomon_prop <- function(obj, prop) {
  if (is.null(obj)) return(NULL)
  if (S7::S7_inherits(obj, CFRashomon)) {
    switch(prop,
      n_intersecting = obj@n_intersecting,
      tree_risks = obj@tree_risks,
      rashomon_bound_multiplier = obj@rashomon_bound_multiplier,
      intersecting_trees = obj@intersecting_trees,
      tree_jsons = obj@tree_jsons,
      intersecting_structures = obj@intersecting_structures,
      K = obj@K,
      fold_indices = obj@fold_indices,
      rashomon_sizes = obj@rashomon_sizes,
      NULL
    )
  } else {
    # S3 object
    obj[[prop]]
  }
}
