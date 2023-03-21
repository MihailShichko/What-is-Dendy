#include <cstdint> 
#include "CPU.h"
#include <array>

class Bus
{   
    public:

        CPU cpu;
        std::array<uint8_t, 64 * 1024> ram;

        Bus();
        ~Bus();

        //bus read and write
        void write(uint16_t addr, uint8_t data);
        uint16_t read(uint16_t addr, bool b_read_only = false);
};