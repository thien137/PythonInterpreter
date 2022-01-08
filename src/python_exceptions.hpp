// Header for python_exceptions.cpp
#include <exception>

struct SyntaxError : public std::exception {
    const char* what() const noexcept {
        return "Syntax Error";
    }
};