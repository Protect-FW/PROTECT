#!/usr/bin/env bash


# Directory where the tree augmentation script is located
script_dir="$(cd "$(dirname "$0")" && pwd)"

# Parent directory
project_dir="$(dirname "$script_dir")"

# Replicates
replicates=$1


for ((rep=1; rep<=replicates; rep++)); do

    # Create taxonomic backbone tree.
    docker run -it \
      -v "$project_dir":/workdir \
      -w /workdir \
      jonchang/tact \
      tact_build_taxonomic_tree \
      /workdir/raw_data/tree_augmentation/all_species.csv \
      --output /workdir/raw_data/tree_augmentation/all_species.tre


    # Add missing species to time-calibrated pyhlogeny and put in the same directory as all the raw data.
    docker run -it \
      -v "$project_dir":/workdir \
      -w /workdir \
      jonchang/tact \
      tact_add_taxa --backbone /workdir/raw_data/tree_augmentation/Hydrobiidae.tre \
      --taxonomy /workdir/raw_data/tree_augmentation/all_species.tre \
      --output /workdir/raw_data/Hydrobiidae_tacted


    # Get a distribution of trees by the given number of replicates
    if [ "$replicates" -gt 1 ]; then
    
        # Move the newick.tre file in a separate folder and rename according to the replicate.
        mv -f \
            "$project_dir/raw_data/Hydrobiidae_tacted.newick.tre" \
            "$project_dir/raw_data/tree_distribution/Hydrobiidae_tacted.newick_${rep}.tre" \
            

        # Delete the The nexus, log, and rates files.
        rm -f "$project_dir/raw_data/Hydrobiidae_tacted.nexus.tre"
        rm -f "$project_dir/raw_data/Hydrobiidae_tacted.log.txt"
        rm -f "$project_dir/raw_data/Hydrobiidae_tacted.rates.csv"
    fi

done


if [ "$replicates" -gt 1 ]; then

    # Convert replicated trees to a multiphylo object
    Rscript "$script_dir/tact_to_multiphylo.R" "$project_dir/raw_data/tree_distribution"

    # Delete replicated trees
    rm -f "$project_dir"/raw_data/tree_distribution/*.tre

fi



