MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# Configuration settings
FC := gfortran
RM := del /Q # For Windows, delete files quietly
SRC := operations_module.f90 utils_module.f90 main.f90 # List of source files
OBJS := $(SRC:.f90=.o) # Replace .f90 with .o
TEST_EXE := complex_calculator.exe # Name of the executable

# Rules
.PHONY: all clean # Define phony targets

all: $(TEST_EXE) # Define the default target

$(TEST_EXE): $(OBJS) # Define the target
	$(FC) -o $@ $^ # Compile the source files


$(OBJS): %.o: %.f90 # Define the rule for compiling source files
	$(FC) -c -o $@ $< # Compile the source file

clean:
	$(RM) $(wildcard *.o) $(TEST_EXE)
	$(RM) *.mod
