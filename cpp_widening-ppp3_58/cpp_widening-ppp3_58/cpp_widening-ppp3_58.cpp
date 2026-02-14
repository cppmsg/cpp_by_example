// cpp_widening-ppp3_58.cpp : Defines the entry point for the application.
#include <iostream>
#include "cpp_widening-ppp3_58.h"
//import std;
using namespace std; // directive

struct Object{int a; double b;};

struct MyStruct { int a; double b; };

int main() {
	int	i = 42;					// copy initialization
	int i2{ 42 };				// direct initialization
    char	c{ 'H' };
    double	d{ 'H' };
    d = c;
    string	s = "Hello CMake.";
	string	s2{ "Hello CMake." };
	MyStruct	ms1 = { 1, 2.0 };			// copy initialization
	MyStruct	ms2{ 1, 2.0 };			// direct initialization
	MyStruct	widening_conversion3{ MyStruct{c,d} };	// direct initialization
	MyStruct	widening_conversion4{ c };			    // direct initialization
	MyStruct	widening_conversion5{ c,c };			    // direct initialization
	MyStruct	widening_conversion7{ c,'H'};			    // direct initialization

	MyStruct	widening_conversion6( c,c );			    // direct initialization

	long	widening_conversion1{ long{c} };			// direct initialization
	long	widening_conversion2{ c };

	long	l{ 'H' };
	//short	auto_conversion7{ short int{l} };			// direct initialization
    short	auto_conversion2{ short (l) };			// direct initialization

	string	widening_s{  string{"cstring"}};	
	string	widening_s2{ string{c} };			

    char c2{'H'};
    Object o2{1, c2};

    cout << widening_conversion1 << widening_conversion2 << widening_s2 << widening_s << "Hello CMake." << endl;
	return 0;
}
