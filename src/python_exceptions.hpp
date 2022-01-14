// Header for python_exceptions.cpp
#include <exception>
#include <assert.h>

struct SyntaxError : public std::exception {
    const char* what() const noexcept {
        return "Syntax Error";
    }
};