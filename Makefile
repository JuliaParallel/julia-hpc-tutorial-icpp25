
# We need to get the absolute path of the makefile (in order to find
# entrypoint.sh) => this is the equivalent of:
# ```
# $(dirname "$(realpath "$source")")
# ```
# in GNUMake
MKFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: adios2 nersc

nersc:
	$(MKFILE_DIR)/opt/bin/simple-templates.ex                    \
		--overwrite "{\"repo_prefix\":\"$(MKFILE_DIR)\"}"        \
		$(MKFILE_DIR)/nersc/templates/activate.sh                \
		$(MKFILE_DIR)/nersc/templates/settings.toml              \
		$(MKFILE_DIR)/{{name}}.sh

	$(MKFILE_DIR)/opt/bin/simple-templates.ex                                \
		--overwrite "{\"repo_prefix\":\"$(MKFILE_DIR)\"}"                    \
		--dir --resource "^jupyter/.*.png$$"                                 \
		--chmod "u+rX,g+rX,o+rX,g-w,o-w" --chmod-basename                    \
		--chmod-overwrite "{\".*/kernel%-helper.sh$$\": \"u+rx,g+rx,o+rx\"}" \
		$(MKFILE_DIR)/nersc/jupyter/template                                 \
		$(MKFILE_DIR)/nersc/templates/settings.toml                          \
		"$(HOME)/.local/share/jupyter/kernels/icpp-tutorial-{{{THREADS_NAME}}}"

adios2:
	$(MKFILE_DIR)/opt/bin/simple-modules.ex  \
		--sm-root $(MKFILE_DIR)/nersc        \
		$(MKFILE_DIR)/nersc/adios2/sm-config