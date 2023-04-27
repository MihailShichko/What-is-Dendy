#include "PPU.h"

void PPU::cpu_write(uint16_t addr, uint8_t data)
{
     switch (addr)
    {
    case 0x0000: //Control
    
        break;    
    case 0x0001: //Mask
    
        break;
    case 0x0002: //Status
    
        break;
    case 0x0003: //OAM Address
    
        break;
    case 0x0004: //OAM Data
    
        break;
    case 0x0005: //Scroll
    
        break;
    case 0x0006: //PPU Address
    
        break;
    case 0x0007: //PPU Data
    
        break;
    }
}

uint8_t PPU::cpu_read(uint16_t addr, bool b_read_only)
{
    uint8_t data = 0x00;

    switch (addr)
    {
    case 0x0000: //Control
    
        break;
    
    case 0x0001: //Mask
    
        break;
    case 0x0002: //Status
    
        break;
    case 0x0003: //OAM Address
    
        break;
    case 0x0004: //OAM Data
    
        break;
    case 0x0005: //Scroll
    
        break;
    case 0x0006: //PPU Address
    
        break;
    case 0x0007: //PPU Data
    
        break;
    }

    return data;
}

void PPU::ppu_write(uint16_t addr, uint8_t data)
{
    addr &=0x3FFF;
}

uint8_t PPU::ppu_read(uint16_t addr, bool b_read_only)
{
    uint8_t data = 0x00;
    addr &=0x3FFF;

    return data;

}
