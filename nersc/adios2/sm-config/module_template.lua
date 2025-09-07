help([[
ADIOS2: The Adaptable Input/Output System version 2
https://adios2.readthedocs.io/en/latest/index.html
]])

whatis("Name: adios2")
whatis("Version: {{{INSTALL_VERSION}}}")
whatis("URL: https://adios2.readthedocs.io/en/latest/index.html")

prereq("PrgEnv-{{{ADIOS2_PE}}}")
prereq("cray-hdf5-parallel")

prepend_path("PATH", "{{{ADIOS2_PATH}}}/bin")
prepend_path("LD_LIBRARY_PATH", "{{{ADIOS2_PATH}}}/lib64")
