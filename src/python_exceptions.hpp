// Header for python_exceptions.cpp
#include <exception>
#include <assert.h>
#include <string>
#include <string.h>

class SyntaxError : public std::exception {
    private:
        std::string message;
    public:
        SyntaxError(const std::string& m) : message{m} {}
        const char* what() const noexcept override {
            return std::strcat("Syntax Error: ", message.c_str());
        }
};