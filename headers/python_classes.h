#include <string>

class PyProcedure {};

class PyClass {};

// Irrelevant Code

class PyString {
    private:
        std::string data;
    public:
        PyString(std::string& s) {data = static_cast<std::string&&>(s);};
};