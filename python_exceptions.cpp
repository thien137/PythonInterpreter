#include "headers/python_exceptions.hpp"

void keyboard_interrupt_handler(int signal) {
    std::cout << "\nKeyboard Interrupt" << std::endl;
    throw KeyboardInterrupt();
}