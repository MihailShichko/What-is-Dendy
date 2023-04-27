#include "Bus.h"

Bus::Bus()
{
    for(auto &i : cpu_ram) i = 0; // clear ram
    cpu.connect_bus(this);
}

Bus::~Bus()
{

}

void Bus::cpu_write(uint16_t addr, uint8_t data)
{
    if(addr >= 0x0000 || addr <= 0x1FFF)
    {
        cpu_ram[addr & 0x07FF] = data;
    }
    else if(addr >= 0x2000 && addr <= 0x3FFF)
    {
        ppu.cpu_write(addr & 0x0007, data);
    }
}

uint16_t Bus::cpu_read(uint16_t addr, bool b_read_only = false)
{
    uint8_t data;

    if(addr >= 0x0000 || addr <= 0x1FFF)
    {
        data = cpu_ram[addr& 0x07FF];
    }
    else if(addr >= 0x2000 && addr <= 0x3FFFF)
    {
        ppu.cpu_read(addr & 0x0007, b_read_only);
    }

    return data;
}