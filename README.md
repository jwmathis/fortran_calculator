## Project Overview
The complex number calculator is a command-line application implemented in Fortran. Fortran was selected as the target language to code the program due to its computational efficiency similar to machine language, which makes it suitable to use for coding a calculator program. 
The calculator supports the basic arithmetic operations: addition, subtraction, multiplication, and division. Users are able to input complex numbers in standard mathematical notation, rectangular (Cartesian) notation, or use it as a regular calculator for real numbers only. 

## Features
* Add, Subtract, Divide, Mulitply, Exponentiate, and find the complex conjugate of real numbers and complex numbers
* Input numbers in mathematical complex number notation and rectangular (Cartesian) notation
* Complex number output is formatted based on your input

## Build the Application
Build the calculator from source:
### Prerequisites
To simplify the process, a Makefile is included in the repository. The Makefile is OS-agnostic, which allows it to be used on any operating system. 
This application requires Fortran compiler and Make build tool.

### Installation
1. Clone the repository
```
git clone https://github.com/jwmathis/fortran_calculator.git
cd complex_calculator
```
3. Run make to compile the code
```
make
```
5. After the build finishes, you can run the executable
```
./complex_calculator
```
6. Clean up the folder
```
make clean
```
