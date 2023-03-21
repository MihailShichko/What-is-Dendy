#include "Bus.h"

Bus::Bus()
{
    for(auto &i : ram) i = 0; // clear ram
    cpu.connect_bus(this);
}

Bus::~Bus()
{

}

void Bus::write(uint16_t addr, uint8_t data)
{
    if(addr >= 0x0000 || addr <= 0xFFFF)
    {
        ram[addr] = data;
    }
}

uint16_t Bus::read(uint16_t addr, bool b_read_only = false)
{
    if(addr >= 0x0000 || addr <= 0xFFFF)
    {
        return ram[addr];
    }

    return 0x00;
}