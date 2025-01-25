MAKEFLAGS += --no-built-in-rules --no-builtin-variables

# Configuration settings
OSFLAG 				:=
ifeq ($(OS),Windows_NT)
	OSFLAG += -D WIN32
	ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
		OSFLAG += -D AMD64
	endif
	ifeq ($(PROCESSOR_ARCHITECTURE),x86)
		OSFLAG += -D IA32
	endif

	RM := del /Q
	EXE_EXT := .exe
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		OSFLAG += -D LINUX
	endif
	ifeq ($(UNAME_S),Darwin)
		OSFLAG += -D OSX
	endif
		UNAME_P := $(shell uname -p)
	ifeq ($(UNAME_P),x86_64)
		OSFLAG += -D AMD64
	endif
		ifneq ($(filter %86,$(UNAME_P)),)
	OSFLAG += -D IA32
		endif
	ifneq ($(filter arm%,$(UNAME_P)),)
		OSFLAG += -D ARM
	endif

	RM := rm -f
	EXE_EXT :=
endif

FC := gfortran
SRC := operations_module.f90 utils_module.f90 main.f90 # List of source files
OBJS := $(SRC:.f90=.o) # Replace .f90 with .o
TEST_EXE := complex_calculator # Name of the executable

# Rules
.PHONY: all clean # Define phony targets

all: $(TEST_EXE) # Define the default target

$(TEST_EXE)$(EXE_EXT): $(OBJS) # Define the target Compile the source files
	$(FC) -o $@ $^

$(OBJS): %.o: %.f90 # Define the rule for compiling source files # Compile the source file
	$(FC) -c -o $@ $< 

clean:
	@echo $(OSFLAG)
	$(info Cleaning...)
	$(RM) $(OBJS) $(wildcard *.mod)
