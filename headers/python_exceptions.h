// Header for python_exceptions.cpp
#include <iostream>
#include <csignal>
#include <sstream>

class KeyboardInterrupt : std::exception {};
void keyboard_interrupt_handler(int);
