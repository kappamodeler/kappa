PREF?= 
VN:=$(shell cat tag/number)#            #Git version number for commit tag
VERSION:=$(shell cat tag/version)#      #Major revision release number
RELEASE:=$(shell cat tag/release)#      #Release number
DATE:=`date +'%Y-%m-%d %H:%M:%S'`#      #date YYYY-MM-DD 


all: simplx_light complx_light 

simplx_light:
	make TKREP=light TKINCLUDES="" TK_CMXA=""  KEY="without_key" simplx
complx_light:
	make TKREP=light TKINCLUDES="" TK_CMXA="" KEY="without_key" complx
shell:
	make TKREP=light TKINCLUDES="" TK_CMXA="" KEY="without_key" interplx

full: simplx_full complx_full 

simplx_full: 
	make simplx TKREP=full TKINCLUDES="-I +labltk" TK_CMXA="labltk.cmxa jpflib.cmxa frxlib.cmxa -cclib -lpthread -cclib -lXau -cclib -lXdmcp" KEY="without_key"

complx_full: 
	make complx TKREP=full TKINCLUDES="-I +labltk" TK_CMXA="labltk.cmxa jpflib.cmxa frxlib.cmxa -cclib -lpthread -cclib -lXau -cclib -lXdmcp" KEY="without_key"

with_key:
	make TKREP=light TKINCLUDES="" TK_CMXQ="" KEY="with_key" simplx
	make TKREP=light TKINCLUDES="" TK_CMXQ="" KEY="with_key" complx

top:
	make toplevel KEY=without_key 

INSTALL_DIR= /usr/bin
LOCAL_DIR?=$(HOME)/fedora/bin
VERSION?=X.YY

NAME?=hopfield
OPTIONS?=
TKREP?=light
SIMPLXREP?=simplx_rep
COMPLXREP?=complx_rep
INTERPLXREP?=interplx_rep

BIN = ./bin
KEY?=without_key

$(BIN): 
	mkdir ./bin

$(SIMPLXREP)/lib:
	mkdir $(SIMPLXREP)/lib 

OCAMLC=		$(OCAMLPREFIX)ocamlc
OCAMLCI=	$(OCAMLPREFIX)ocamlc
OCAMLOPT= 	$(OCAMLPREFIX)ocamlopt.opt
OCAMLYACC=	$(OCAMLPREFIX)ocamlyacc -v
OCAMLLEX=	$(OCAMLPREFIX)ocamllex

TKINCLUDES? = 

OCAMLINCLUDES= -I $(COMPLXREP)/lib/$(TKREP) \
		-I $(COMPLXREP)/lib/$(KEY) \
		-I $(COMPLXREP)/backend/latex \
		-I $(COMPLXREP)/automatically_generated/ \
		-I $(COMPLXREP)/backend \
	        -I $(COMPLXREP)/config \
	        -I $(COMPLXREP)/tools \
	        -I $(COMPLXREP)/expr  \
	        -I $(COMPLXREP)/compressor \
                -I $(COMPLXREP)/data_structures \
                -I $(COMPLXREP)/reachability \
                -I $(COMPLXREP)/vars \
	        -I $(COMPLXREP)/abstract_expr \
                -I $(COMPLXREP)/frontend \
                -I $(COMPLXREP)/backend/parse_comment \
                -I $(COMPLXREP)/backend/contact_map \
                -I $(COMPLXREP)/lib/ \
		-I $(SIMPLXREP)/src/bnf \
		-I $(SIMPLXREP)/src/data_structures \
		-I $(SIMPLXREP)/src/kappa \
		-I $(SIMPLXREP)/src/main \
		-I $(SIMPLXREP)/src/stories \
		-I $(SIMPLXREP)/src/html_config/ \
		-I $(SIMPLXREP)/src/tools/ \
                -I $(SIMPLXREP)/lib \
		-I $(COMPLXREP)/influence_map \
                -I $(COMPLXREP)/pipeline \
                -I $(COMPLXREP)/share \
                -I $(COMPLXREP)/backend/XML \
		-I $(COMPLXREP)/backend/HTML \
		-I $(COMPLXREP)/ODE \
		-I $(COMPLXREP)/cyclical_complexes \
		-I $(COMPLXREP)/refinements \
		-I $(COMPLXREP)/isomorphism_detection \
		$(TKINCLUDES) 

OCAMLFLAGS=	$(OCAMLINCLUDES)
OCAMLLIBDIR=	$(shell ocamlc -where)
CFLAGS=		-I $(OCAMLLIBDIR) -Wall -Werror -Wno-unused -DPENTIUM_III_COMPATIBLE

# TK
TK_CMA?  = labltk.cma  jpflib.cma  frxlib.cma
TK_CMXA? = 

LIBS_MLFILES = #$(COMPLXREP)/lib/wordexp.ml
LIBS_CMOFILES = $(LIBS_MLFILES:%.ml=%.cmo)
LIBS_CMXFILES = $(LIBS_MLFILES:%.ml=%.cmx)

#LIBS_CFILES =  #$(COMPLXREP)/lib/ml_wordexp.c
#LIBS_OFILES = $(LIBS_CFILES:%.c=%.o)

#LIBSC_CMA = $(COMPLXREP)/lib/libs.cma
#LIBSC_CMXA = $(COMPLXREP)/lib/libs.cmxa

#$(LIBSC_CMA): $(LIBS_CMOFILES) $(LIBS_OFILES)
#	$(OCAMLC) -a -o $@ $+

#$(LIBSC_CMXA): $(LIBS_CMXFILES) $(LIBS_OFILES)
#	$(OCAMLOPT) -a -o $@ $+

AUTOGENML=$(SIMPLXREP)/src/bnf/kappa_parse.ml 

AUTODURINGCOMMIT=$(COMPLXREP)/automatically_generated/svn_number.ml 

MLFULL? = $(COMPLXREP)/lib/$(TKREP)/superargTk.ml 

TKFILE=

OBJS = 	./$(COMPLXREP)/automatically_generated/git_commit_info.cmo  \
	./$(COMPLXREP)/tools/exceptions.cmo \
	./$(COMPLXREP)/tools/memory_usage.cmo \
	./$(COMPLXREP)/lib/$(KEY)/key.cmo \
	./$(SIMPLXREP)/src/tools/float_pretty_printing.cmo \
	./$(COMPLXREP)/tools/error_handler_common.cmo \
	./$(SIMPLXREP)/src/tools/error.cmo \
	./$(SIMPLXREP)/src/html_config/config.cmo \
	./$(COMPLXREP)/tools/tools2.cmo \
	./$(COMPLXREP)/tools/map2.cmo \
	./$(COMPLXREP)/tools/map_random.cmo \
	./$(COMPLXREP)/tools/map_with_sharing.cmo \
	./$(COMPLXREP)/tools/unbounded_array.cmo \
	./$(COMPLXREP)/backend/parse_comment/comment_sig.cmo \
	./$(COMPLXREP)/frontend/pb_sig.cmo \
	./$(COMPLXREP)/data_structures/data_structures.cmo \
	./$(COMPLXREP)/lib/superarg.cmo \
	./$(COMPLXREP)/lib/$(TKREP)/superargTk.cmo \
	./$(COMPLXREP)/data_structures/big_array.cmo \
	./$(SIMPLXREP)/src/data_structures/array_ext.cmo\
	./$(SIMPLXREP)/src/data_structures/random_tree.cmo \
	./$(SIMPLXREP)/src/tools/stringlist.cmo \
	./$(SIMPLXREP)/src/kappa/agent.cmo \
	./$(SIMPLXREP)/src/data_structures/coord.cmo \
	./$(SIMPLXREP)/src/data_structures/map_ext.cmo \
	./$(SIMPLXREP)/src/data_structures/longString.cmo \
	./$(SIMPLXREP)/src/data_structures/val_map.cmo \
	./$(SIMPLXREP)/src/tools/mods2.cmo \
	./$(SIMPLXREP)/src/data_structures/paths.cmo \
	./$(SIMPLXREP)/src/kappa/solution.cmo \
	./$(SIMPLXREP)/src/kappa/species.cmo \
	./$(SIMPLXREP)/src/data_structures/implementation_choices.cmo \
	./$(SIMPLXREP)/src/kappa/rule.cmo \
	./$(SIMPLXREP)/src/kappa/experiment.cmo \
	./$(SIMPLXREP)/src/main/data.cmo \
	./$(COMPLXREP)/config/config_complx.cmo \
	./$(COMPLXREP)/tools/error_handler.cmo \
	./$(SIMPLXREP)/src/stories/network.cmo \
	./$(SIMPLXREP)/src/stories/story_compressor.cmo \
	./$(COMPLXREP)/backend/latex/latex.cmo \
	./$(COMPLXREP)/tools/tools.cmo \
	./$(SIMPLXREP)/src/stories/iso.cmo \
	./$(COMPLXREP)/share/share.cmo \
	./$(COMPLXREP)/data_structures/hash.cmo \
	./$(COMPLXREP)/backend/contact_map/acyclicity.cmo \
        ./$(COMPLXREP)/reachability/concretization.cmo \
	./$(COMPLXREP)/reachability/count_complexes.cmo \
	./$(COMPLXREP)/vars/var.cmo \
	./$(COMPLXREP)/expr/expr.cmo \
	./$(COMPLXREP)/isomorphism_detection/count_isomorphism.cmo \
	./$(COMPLXREP)/expr/kleenean_expr.cmo \
	./$(COMPLXREP)/abstract_expr/abstract_expr_sig.cmo \
	./$(COMPLXREP)/abstract_expr/bdd.cmo \
	./$(COMPLXREP)/abstract_expr/rough.cmo \
	./$(COMPLXREP)/abstract_expr/partition.cmo \
	./$(COMPLXREP)/backend/parse_comment/yacc.cmo \
	./$(COMPLXREP)/backend/parse_comment/lexeur.cmo \
	./$(COMPLXREP)/reachability/contact_map.cmo \
	./$(COMPLXREP)/reachability/reachability.cmo \
	./$(COMPLXREP)/frontend/cbng_sig.cmo \
	./$(COMPLXREP)/frontend/cbng.cmo \
	./$(SIMPLXREP)/src/bnf/kappa_parse.cmo \
        ./$(SIMPLXREP)/src/bnf/kappa_lex.cmo \
	./$(COMPLXREP)/compressor/compressor.cmo \
	./$(COMPLXREP)/frontend/translate.cmo \
	./$(COMPLXREP)/reachability/packing.cmo \
	./$(COMPLXREP)/influence_map/quarkification.cmo \
	./$(COMPLXREP)/backend/contact_map/neighborhood.cmo \
	./$(COMPLXREP)/backend/contact_map/output_contact_map.cmo \
	./$(COMPLXREP)/cyclical_complexes/avoid_polymere.cmo \
	./$(COMPLXREP)/refinements/refinements.cmo \
	./$(COMPLXREP)/backend/XML/xml.cmo \
	./$(COMPLXREP)/backend/HTML/html.cmo \
	./$(COMPLXREP)/influence_map/influence_map.cmo \
	./$(COMPLXREP)/ODE/ode_print_sig.cmo \
	./$(COMPLXREP)/ODE/arithmetics.cmo \
	./$(COMPLXREP)/ODE/ode_print.cmo \
	./$(COMPLXREP)/ODE/annotated_contact_map.cmo \
	./$(COMPLXREP)/ODE/annotated_contact_map_stoc.cmo \
	./$(COMPLXREP)/ODE/views.cmo \
	./$(COMPLXREP)/ODE/rooted_path.cmo \
	./$(COMPLXREP)/ODE/fragments_sig.cmo \
	./$(COMPLXREP)/ODE/fragments.cmo \
	./$(COMPLXREP)/ODE/ode_computation.cmo \
	./$(COMPLXREP)/ODE/reaction_computation.cmo \
	./$(COMPLXREP)/backend/contact_map/find_cycles.cmo \
	./$(COMPLXREP)/backend/contact_map/connected_components.cmo \
	./$(COMPLXREP)/pipeline/pipeline.cmo \
	./$(SIMPLXREP)/src/main/session.cmo \
	./$(SIMPLXREP)/src/main/memory_control.cmo \
	./$(SIMPLXREP)/src/tools/bench.cmo \
	./$(SIMPLXREP)/src/main/simulation2.cmo \
	./$(SIMPLXREP)/src/main/monitor.cmo \
	./$(SIMPLXREP)/src/html_config/HTML.cmo \
	./$(SIMPLXREP)/src/main/time_course.cmo

SIMPLX_MAIN = ./$(SIMPLXREP)/src/main/main.ml
COMPLX_MAIN = ./$(COMPLXREP)/main.ml 
INTERPLX_MAIN = ./$(INTERPLXREP)/shell.ml


NATIVE_OBJS = $(OBJS:cmo=cmx) 
MLFILES = $(OBJS:cmo=ml) $(SIMPLX_MAIN) $(COMPLX_MAIN)

MLI =  ./$(SIMPLXREP)/src/tools/error.mli \
	./$(SIMPLXREP)/src/data_structures/coord.mli \
	./$(SIMPLXREP)/src/data_structures/map_ext.mli \
	./$(SIMPLXREP)/src/data_structures/array_ext.mli \
	./$(SIMPLXREP)/src/data_structures/longString.mli \
	./$(SIMPLXREP)/src/tools/mods2.mli \
	./$(SIMPLXREP)/src/data_structures/paths.mli \
        ./$(SIMPLXREP)/src/kappa/agent.mli \
	./$(SIMPLXREP)/src/kappa/solution.mli \
	./$(SIMPLXREP)/src/kappa/rule.mli\
	./$(SIMPLXREP)/src/kappa/experiment.mli\
	./$(SIMPLXREP)/src/bnf/kappa_parse.mli \

CMI = $(MLI:mli=cmi)
CMA = str.cma nums.cma unix.cma #threads.cma
CMXA = unix.cmxa str.cmxa nums.cmxa #threads.cmxa

SIMPLX_OUT = simplx
COMPLX_OUT = complx
OUTPUT = $(SIMPLX_OUT) $(COMPLX_OUT) 

LIB_OPT = $(SIMPLX_OUT).cmxa
LIB_BYTE = $(SIMPLX_OUT).cma

DOCS = $(MLI) $(OBJS:cmo=ml)
DOCREP = ./document
DOCTYPE = 

gen_doc :  
	ocamldoc -$(DOCTYPE) $(DOCS) $(OCAMLINCLUDES) -d $(DOCREP) 

html_doc : 
	make KEY=without_key DOCTYPE=html gen_doc

dot_doc :
	make KEY=without_key DOCTYPE=dot gen_doc

simplx :$(MLI) $(CMI) $(LIBSC_CMXA) $(LIB_OPT) $(SIMPLX_MAIN) $(BIN)
	$(OCAMLOPT) $(OCAMLFLAGS) $(CMXA) $(TK_CMXA) $(LIBSC_CMXA) $(LIB_OPT)  $(SIMPLX_MAIN) -o $(BIN)/$(SIMPLX_OUT)

byte :  $(MLI) $(CMI) $(LIBSC_CMA) $(LIB_BYTE) $(SIMPLX_MAIN) $(BIN)
	$(OCAMLC) $(OCAMLFLAGS) $(CMA) $(LIBSC_CMA) $(LIB_BYTE) $(SIMPLX_MAIN) -o $(BIN)/$(SIMPLX_OUT).byte

dep :  
	ocamldep $(OCAMLINCLUDES) $(MLFILES)

$(SIMPLX_OUT).cmxa: $(MLI) $(CMI) $(LIBSC_CMXA) $(NATIVE_OBJS)  $(SIMPLXREP)/lib
	$(OCAMLOPT) $(OCAMLFLAGS) -a $(NATIVE_OBJS)  -o $(SIMPLXREP)/lib/$(SIMPLX_OUT).cmxa

$(SIMPLX_OUT).cma: $(MLI) $(CMI) $(LIBSC_CMA) $(OBJS) $(LIB) $(SIMPLXREP)/lib 
	$(OCAMLC) $(OCAMLFLAGS) -a $(OBJS) -o $(SIMPLXREP)/lib/$(SIMPLX_OUT).cma

LINE = $(OCAMLOPT) $(OCAMLFLAGS) $(TKINCLUDES) $(CMXA) $(TK_CMXA) $(LIBSC_CMXA)  $(NATIVE_OBJS) 

complx: $(LIBSC_CMXA) $(NATIVE_OBJS) $(COMPLX_MAIN) $(BIN)
	$(LINE) $(COMPLX_MAIN) -o $(BIN)/$(COMPLX_OUT)

toplevel: $(MLI) $(CMI) $(LIBSC_CMA) $(LIB_BYTE)
	ocaml -I +threads $(OCAMLINCLUDES) $(CMA) $(OBJS)

toplx: $(MLI) $(CMI) $(LIBSC_CMA) $(LIB_BYTE)
	ocamlmktop -o toplx -custom $(OCAMLINCLUDES) -cclib -lunix -cclib -lnums thread.cma unix.cma nums.cma $(LIB_BYTE)

./$(SIMPLXREP)/src/bnf/kappa_parse.ml ./$(SIMPLXREP)/src/bnf/kappa_parse.mli : ./$(SIMPLXREP)/src/bnf/kappa_parse.mly
	ocamlyacc ./$(SIMPLXREP)/src/bnf/kappa_parse.mly 

./$(COMPLXREP)/automatically_generated/svn_number.ml:
	make grab_svn_version_number

./$(SIMPLXREP)/src/bnf/kappa_parse.cmo: ./$(SIMPLXREP)/src/bnf/kappa_parse.mli ./$(SIMPLXREP)/src/bnf/kappa_parse.ml
	$(OCAMLC) $(OCAMLFLAGS) -c ./$(SIMPLXREP)/src/bnf/kappa_parse.mli ./$(SIMPLXREP)/src/bnf/kappa_parse.ml

./$(SIMPLXREP)/src/bnf/kappa_lex.ml: ./$(SIMPLXREP)/src/bnf/kappa_lex.mll
	ocamllex ./$(SIMPLXREP)/src/bnf/kappa_lex.mll

./$(COMPLXREP)/backend/parse_comment/yacc.ml: ./$(COMPLXREP)/backend/parse_comment ./$(COMPLXREP)/backend/parse_comment/yacc.mly 
	ocamlyacc ./$(COMPLXREP)/backend/parse_comment/yacc.mly

./$(COMPLXREP)/backend/parse_comment/yacc.cmx: ./$(COMPLXREP)/backend/parse_comment/yacc.cmi ./$(COMPLXREP)/backend/parse_comment/yacc.ml ./$(COMPLXREP)/backend/parse_comment/yacc.mly
	$(OCAMLOPT) -c $(OCAMLFLAGS) ./$(COMPLXREP)/backend/parse_comment/yacc.ml

./$(COMPLXREP)/backend/parse_comment/yacc.cmo: ./$(COMPLXREP)/backend/parse_comment/yacc.cmi ./$(COMPLXREP)/backend/parse_comment/yacc.ml ./$(COMPLXREP)/backend/parse_comment/yacc.mly
	$(OCAMLC) -c $(OCAMLFLAGS) ./$(COMPLXREP)/backend/parse_comment/yacc.ml


./$(COMPLXREP)/backend/parse_comment/yacc.cmi: ./$(COMPLXREP)/backend/parse_comment/yacc.mli2
	cp ./$(COMPLXREP)/backend/parse_comment/yacc.mli2 ./$(COMPLXREP)/backend/parse_comment/yacc.mli;
	$(OCAMLOPT) -c $(OCAMLFLAGS) ./$(COMPLXREP)/backend/parse_comment/yacc.mli

./$(COMPLXREP)/backend/parse_comment/lexeur.ml: ./$(COMPLXREP)/backend/parse_comment/lexeur.mll
	ocamllex ./$(COMPLXREP)/backend/parse_comment/lexeur.mll

%.cmi : %.mli
	$(OCAMLC) $(OCAMLFLAGS) $<

%.cmo : %.ml 
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx : %.ml 
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

install: bin/simplx bin/complx
	sudo ln -sf $(PWD)/bin/* $(INSTALL_DIR) 

install_in_local: bin/simplx bin/complx
	 ln -sf $(PWD)/bin/* $(LOCAL_DIR) 

uninstall_of_local: clean
	cd $(LOCAL_DIR) ; rm -f $(OUTPUT)

uninstall: clean
	cd $(INSTALL_DIR) ; sudo rm -f $(OUTPUT)

$(HOME)/tmp: 
	mkdir $(HOME)/tmp 

tar:tar_prorep
tar_prorep: $(HOME)/tmp
	make clean_all 
	rm -rf $(HOME)/tmp/ProRepPlx-$(VERSION)
	mkdir $(HOME)/tmp/ProRepPlx-$(VERSION)
	cp -r * $(HOME)/tmp/ProRepPlx-$(VERSION)/
	cd $(HOME)/tmp ; tar czf ProRepPlx-$(VERSION).tgz ProRepPlx-$(VERSION)/*
	cp $(HOME)/tmp/ProRepPlx-$(VERSION).tgz $(HOME)/
	rm $(HOME)/tmp/ProRepPlx-$(VERSION).tgz 



install_light:
	cd $(SIMPLXREP) ; make install
	cd $(COMPLXREP) ; make install_light
	cd $(INTERPLXREP) ; make install_light

clean:
	rm -f *~ ; 
	cd $(SIMPLXREP) ; make -f cleanup ;
	cd $(COMPLXREP) ; make -f cleanup ; 
	cd $(INTERPLXREP) ; make -f cleanup ;


clean_all: clean 
	rm -f $(AUTOGENML) ;
	rm -f simplx_rep/sim complx_rep/compress complx_rep/compress_light  simplx complx_light bd_influence_map bd_influence_map_light complx *.options* $(OUTPUT)

grab_svn_version_number:
	svn up --username hudson --password bu1ldme --no-auth-cache | tail -n 1 | sed -e "s/\([^0-9]*\)\([0-9]*\)\./let svn_number = \2 +1/" > complx_rep/automatically_generated/svn_number.ml 


inc_svn:
	echo -n `expr $(VN) + 1`> tag/number 

inc_version:
	echo -n `expr $(VERSION) +1`> tag/version

inc_release: 
	echo -n `expr $(RELEASE) +1`> tag/release

fetch_version:
	cd tag ; git pull 

arch_object:

commit:
	make fetch_version
	echo -n `expr $(VN) + 1` > tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Not a release" send_caml

major_version: 
	make fetch_version
	echo -n `expr $(VERSION) + 1` > tag/version
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n 1 > tag/release
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

release: 
	make fetch_version
	echo -n `expr $(RELEASE) + 1`> tag/release
	echo -n `expr $(VN) + 1`> tag/number 
	echo -n $(DATE) > tag/date 
	make PREF="Release " send_caml

send_caml: 
	echo -n xxx$(VN)$(RELEASE)$(VERSION)$(DATE)xxx
	echo -n let git_commit_version,git_commit_release,git_commit_tag,git_commit_date  = $(VERSION),$(RELEASE),$(VN),\"$(DATE)\" > complx_rep/automatically_generated/git_commit_info.ml 
	git commit -a 
	git tag -a $(VN)  -m "$(PREF) v$(VERSION).$(RELEASE)...$(VN) $(DATE)"  
	git push --tags
	git push 

arch_object:
	file $(BIN)/complx | perl -pe '$$uname = `uname -s`; chomp($$uname); s/^.*\s([0-9]*-bit).*$$/binaries\/plx_engine\/$$uname-$$1/g' > tag/arch_object

upload:
	# make release
	make arch_object
	support/s3sync/s3cmd.rb put plectix-deploy:$(shell cat tag/arch_object)/latest tag/number
	support/s3sync/s3cmd.rb put plectix-deploy:$(shell cat tag/arch_object)/$(VN)/complx bin/complx
	support/s3sync/s3cmd.rb put plectix-deploy:$(shell cat tag/arch_object)/$(VN)/simplx bin/simplx

help: 
	@echo Usage: ;\
	echo make all: create the simulator sim and the compressor compress and the meta-language preprocessor ;\
	echo make sim: create the simulator ;\
	echo make complx_full: create the compressor;\
	echo make complx_light: create the light version of the compressor without labltk;\
	echo make commit: update config file with the tag counter before doing a commit&push;\
	echo make release: update the tag counter and the release counter before doing a commit&push;\
	echo make version: update the tag counter and the version counter, reset the release counter, before doing a commit&push;\
	echo make clean: clean compiled files;\
	echo make clean_data: clean analysis results;\
	echo make clean_all: clean all	
