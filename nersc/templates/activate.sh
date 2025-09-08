
__PREFIX__={{{repo_prefix}}}

if [[ -z "${__PREFIX__}" ]]
then
    echo "__PREFIX__ not set"
    return 1
fi

if [[ ${__PREFIX__} == "INVALID" ]]
then
    echo "__PREFIX__ set to invalid value"
    return 1
fi

ml load julia

export JULIA_PROJECT=${__PREFIX__}
export JULIA_DEPOT_PATH=${SCRATCH}/depot

ml use {{{GLOBAL_TUTORIAL_REPO_DIR}}}/nersc/modules/
ml load cray-hdf5-parallel
ml load adios2/${PE_ENV,,}