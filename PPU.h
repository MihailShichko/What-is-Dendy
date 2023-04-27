#include <cstdint> 
#include <array>


class PPU
{
    public:
        PPU();
        ~PPU();

    //Communication with CPU bus
        void cpu_write(uint16_t addr, uint8_t data);
        uint8_t cpu_read(uint16_t addr, bool b_read_only = false);
    
    //Communication with PPU bus
        void ppu_write(uint16_t addr, uint8_t data);
        uint8_t ppu_read(uint16_t addr, bool b_read_only = false);
        

};