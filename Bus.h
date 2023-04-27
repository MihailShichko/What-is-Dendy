#include <cstdint> 
#include <array>
#include <memory>

#include "CPU.h"
#include "PPU.h"
#include "Cartridge.h"

class Bus
{   
    public:

        CPU cpu;
        std::array<uint8_t, 2048> cpu_ram;

        PPU ppu;

        Bus();
        ~Bus();

        //bus cpu_read and cpu_write
        void cpu_write(uint16_t addr, uint8_t data);
        uint16_t cpu_read(uint16_t addr, bool b_read_only = false);

        //System Interface
        void insert_Cartridge(const std::shared_ptr<Cartridge>& cartridge);
        void reset();
        void clock();
    
    private:
        //count of how many clocks have passed
        uint32_t clock_counter = 0;


};