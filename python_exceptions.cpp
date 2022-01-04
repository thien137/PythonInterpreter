#include "headers/python_exceptions.h"

void keyboard_interrupt_handler(int signal) {
    std::cout << "\nKeyboard Interrupt" << std::endl;
    throw KeyboardInterrupt();
}