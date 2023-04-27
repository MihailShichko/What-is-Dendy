#include "Cartridge.h"

void Cartridge::cpu_write(uint16_t addr, uint8_t data)
{
}

uint8_t Cartridge::cpu_read(uint16_t addr, bool b_read_only)
{
    return 0;
}

void Cartridge::ppu_write(uint16_t addr, uint8_t data)
{
}

uint8_t Cartridge::ppu_read(uint16_t addr, bool b_read_only)
{
    return 0;
}
