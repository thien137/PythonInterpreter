#include <cmath>
#include <string>
#include <memory>
#include "headers/python_tokens.hpp"

bool is_atomic(Token t);
bool is_true(Token t);

template <class T>
std::unique_ptr<T> make_unique(Args&&... args);
