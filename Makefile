TARGETS = ocaml-wget owget

all:
	ocp-build -init -scan $(TARGETS)

clean:
	rm -rf _obuild
	rm -f ocp-build.root ocp-build.root.old