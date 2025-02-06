#pragma once

#include <iostream>
#include <string>
#include <sstream>

// Helper function that take a line number and any number of additional args
// that it uses to write an error message and terminate the program.
template <typename... Ts>
void Error(size_t line_num, Ts... message) {
  std::cerr << "ERROR (line " << line_num <<  "): ";
  (std::cerr << ... << std::forward<Ts>(message)) << std::endl;
  exit(1);
}

// Helper that converts everything passed to it into a single, concatenated string.
template <typename... Ts>
std::string ToString(Ts... message) {
  std::stringstream ss;
  (ss << ... << std::forward<Ts>(message));
  return ss.str();
}

// Helper to test if the input is equal to any of a set of specified values.
// For example, IsOneOf<10,25,42>(x) will test if x is equal to any of 10, 25, or 42.
// (A set of lexer ID's can be put into the template to test if a token is any of them.)
template <auto... VALS, typename T>
bool IsOneOf(T value) {
  return ((value == VALS) || ...);
}
