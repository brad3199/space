#include <iostream>
#include <fstream>
#include <SDL2/SDL.h>

#define windowWidth 224*3
#define windowHeight 256*3

#define colour 1
#define testrom 0
#define debug 0

std::ifstream romfile;
bool iff;
char buffer[65536];
int16_t s16;
uint8_t opcode, memory[65536] = {}, mirror[0x1FFF], nextByte, temp8, screenBuffer[256][224][4], shiftOffset;
uint16_t pc, sp, nextWord, temp16, shift, timer, cycles;
uint32_t temp32;

SDL_Event event;
SDL_Renderer *renderer;
SDL_Texture *texture;
SDL_TimerID clocktimer, int1timer, int2timer;
SDL_Window *window;

static const uint8_t opcodeCycles[] = {
//  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    4,  10, 7,  5,  5,  5,  7,  4,  4,  10, 7,  5,  5,  5,  7,  4,  // 0
    4,  10, 7,  5,  5,  5,  7,  4,  4,  10, 7,  5,  5,  5,  7,  4,  // 1
    4,  10, 16, 5,  5,  5,  7,  4,  4,  10, 16, 5,  5,  5,  7,  4,  // 2
    4,  10, 13, 5,  10, 10, 10, 4,  4,  10, 13, 5,  5,  5,  7,  4,  // 3
    5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  // 4
    5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  // 5
    5,  5,  5,  5,  5,  5,  7,  5,  5,  5,  5,  5,  5,  5,  7,  5,  // 6
    7,  7,  7,  7,  7,  7,  7,  7,  5,  5,  5,  5,  5,  5,  7,  5,  // 7
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,  // 8
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,  // 9
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,  // A
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,  // B
    5,  10, 10, 10, 11, 11, 7,  11, 5,  10, 10, 10, 11, 11, 7,  11, // C
    5,  10, 10, 10, 11, 11, 7,  11, 5,  10, 10, 10, 11, 11, 7,  11, // D
    5,  10, 10, 18, 11, 11, 7,  11, 5,  5,  10, 5,  11, 11, 7,  11, // E
    5,  10, 10, 4,  11, 11, 7,  11, 5,  5,  10, 4,  11, 11, 7,  11  // F
};

static const char* opcodeTable[] = {"nop", "lxi b,#", "stax b", "inx b",
    "inr b", "dcr b", "mvi b,#", "rlc", "ill", "dad b", "ldax b", "dcx b",
    "inr c", "dcr c", "mvi c,#", "rrc", "ill", "lxi d,#", "stax d", "inx d",
    "inr d", "dcr d", "mvi d,#", "ral", "ill", "dad d", "ldax d", "dcx d",
    "inr e", "dcr e", "mvi e,#", "rar", "ill", "lxi h,#", "shld", "inx h",
    "inr h", "dcr h", "mvi h,#", "daa", "ill", "dad h", "lhld", "dcx h",
    "inr l", "dcr l", "mvi l,#", "cma", "ill", "lxi sp,#", "sta $", "inx sp",
    "inr M", "dcr M", "mvi M,#", "stc", "ill", "dad sp", "lda $", "dcx sp",
    "inr a", "dcr a", "mvi a,#", "cmc", "mov b,b", "mov b,c", "mov b,d",
    "mov b,e", "mov b,h", "mov b,l", "mov b,M", "mov b,a", "mov c,b", "mov c,c",
    "mov c,d", "mov c,e", "mov c,h", "mov c,l", "mov c,M", "mov c,a", "mov d,b",
    "mov d,c", "mov d,d", "mov d,e", "mov d,h", "mov d,l", "mov d,M", "mov d,a",
    "mov e,b", "mov e,c", "mov e,d", "mov e,e", "mov e,h", "mov e,l", "mov e,M",
    "mov e,a", "mov h,b", "mov h,c", "mov h,d", "mov h,e", "mov h,h", "mov h,l",
    "mov h,M", "mov h,a", "mov l,b", "mov l,c", "mov l,d", "mov l,e", "mov l,h",
    "mov l,l", "mov l,M", "mov l,a", "mov M,b", "mov M,c", "mov M,d", "mov M,e",
    "mov M,h", "mov M,l", "hlt", "mov M,a", "mov a,b", "mov a,c", "mov a,d",
    "mov a,e", "mov a,h", "mov a,l", "mov a,M", "mov a,a", "add b", "add c",
    "add d", "add e", "add h", "add l", "add M", "add a", "adc b", "adc c",
    "adc d", "adc e", "adc h", "adc l", "adc M", "adc a", "sub b", "sub c",
    "sub d", "sub e", "sub h", "sub l", "sub M", "sub a", "sbb b", "sbb c",
    "sbb d", "sbb e", "sbb h", "sbb l", "sbb M", "sbb a", "ana b", "ana c",
    "ana d", "ana e", "ana h", "ana l", "ana M", "ana a", "xra b", "xra c",
    "xra d", "xra e", "xra h", "xra l", "xra M", "xra a", "ora b", "ora c",
    "ora d", "ora e", "ora h", "ora l", "ora M", "ora a", "cmp b", "cmp c",
    "cmp d", "cmp e", "cmp h", "cmp l", "cmp M", "cmp a", "rnz", "pop b",
    "jnz $", "jmp $", "cnz $", "push b", "adi #", "rst 0", "rz", "ret", "jz $",
    "ill", "cz $", "call $", "aci #", "rst 1", "rnc", "pop d", "jnc $", "out p",
    "cnc $", "push d", "sui #", "rst 2", "rc", "ill", "jc $", "in p", "cc $",
    "ill", "sbi #", "rst 3", "rpo", "pop h", "jpo $", "xthl", "cpo $", "push h",
    "ani #", "rst 4", "rpe", "pchl", "jpe $", "xchg", "cpe $", "ill", "xri #",
    "rst 5", "rp", "pop psw", "jp $", "di", "cp $", "push psw", "ori #",
    "rst 6", "rm", "sphl", "jm $", "ei", "cm $", "ill", "cpi #", "rst 7"};

struct {

	uint8_t a;

	union {
		struct {
			uint8_t c;
			uint8_t b;
		}; 
	uint16_t bc;
	};
	
	union {
		struct {
			uint8_t e;
			uint8_t d;
		};
	uint16_t de;
	};

	union {
		struct {
			uint8_t l;
			uint8_t h;
		};
	uint16_t hl;
	};

} reg;

static union{
	struct{
		uint8_t c:1, :1, p:1, :1, ac:1, :1, z:1, s:1;
	} flag;
		uint8_t flags;
};

void init() {

	window = SDL_CreateWindow("invaders", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, windowWidth, windowHeight, SDL_WINDOW_OPENGL);
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
	texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, 224, 256);

        if (!testrom) {
	
		romfile.open("roms/invaders.rom");
        	romfile.read(buffer, 8192);

		for (int i=0; i < 65536; i++) memory[i] = buffer[i];

	}

	else {
	
		//romfile.open("cpudiag.bin");
		//romfile.read(buffer, 1453);

		romfile.open("CPUTEST.COM");
		romfile.read(buffer, 19200);
		
		for (int i=0; i < 19200; i++) memory[i+0x100] = buffer[i];

		pc=0x0100;
		memory[0x5] = 0xC9;

		//memory[0x11B] = 0xC3;
		//memory[0x124] = 0xc3;

		/*memory[0x59c] = 0xc3; // Skip DAA test
		memory[0x59d] = 0xc2;
		memory[0x59e] = 0x05;*/

	}
}

void dumpRegs() {

          std::cout << std::hex << std::uppercase << "a: " << (int)reg.a << " b: " << (int)reg.b << " c: " << (int)reg.c << " d: " << (int)reg.d << " e: " <<
          (int)reg.e << " h: " << (int)reg.h << " l: " << (int)reg.l << " bc: " << (int)reg.bc << " de: " << (int)reg.de << " hl: " << (int)reg.hl <<
          " c: " << (int)flag.c << " p: " << (int)flag.p << " s: " << (int)flag.s << " z: " << (int)flag.z << " sp: $" << std::setw(4) << (int)sp << std::endl;

}

int calcTwos(uint8_t value) {

	value = ~value;
	value++;

	return value;

}

void readSerial() {

	if (memory[0xFF02] == 0x81) {
		
		std::cout << (char)memory[0xFF01] << std::endl;
		memory[0xFF01] = 0;

	}
}

void generateInterrupt(uint8_t interruptNum) {

	iff=0;
	
	memory[sp-1] = pc >> 8; memory[sp-2] = pc & 0x00FF; // PUSH PC
	sp-=2;

	pc=interruptNum * 8;

}

void setZSP(uint8_t value) {

	uint8_t one_bits = 0;
	
	if (value == 0) flag.z=1; else flag.z=0;
	if (value & 0x80) flag.s=1; else flag.s=0;

	for (int i = 0; i < 8; i++) { 
		one_bits += ((value >> i) & 1);
	}

	flag.p = (one_bits & 1) == 0;

}

void Call(uint8_t value) {

	if (testrom && value == 5) {

		if (reg.c == 2) printf("%c", reg.e);
		
		if (reg.c == 9) {

			temp8=0;
			
			while ((char)memory[reg.de+temp8] != '$') {
			
				std::cout << (char)memory[reg.de+temp8];
				temp8++;

			}
		}

	pc+=3;

	}

	else pc+=3; memory[sp-1] = pc >> 8; memory[sp-2] = pc; sp-=2; pc=nextWord;
	//std::cout << std::hex << std::uppercase << "Call: " << (int)memory[sp-1] << (int)memory[sp-2] << std::endl;
	

}


void Return() {

	//std::cout << std::hex << std::uppercase << "Return: " << "memory[sp+1] = " << ((int)memory[sp+1] << 8) << "memory[sp] = " << (int)memory[sp] << std::endl;
	pc = memory[sp+1] << 8 | memory[sp]; sp+=2;

}

int Rst(int value) {

	value = (value >> 3) & 7;
	
	return value * 8;

}

void In(uint8_t value) {

	switch(value) {

		reg.a=0;
		
		case 1: 

				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_c) reg.a |= 1;
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_2) reg.a |= 2;
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_1) reg.a |= 4;
				reg.a |= 8; // Always 1
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_SPACE) reg.a |= 16;
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_LEFT) reg.a |= 32;
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_RIGHT) reg.a |= 64;
				// NC
				break;

		case 2:	
			
				reg.a &= 1; // DIP3 00 = 3 ships 10 = 5 ships
				reg.a &= 2; // DIP4 01 = 4 ships 11 = 6 ships
				if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_t) reg.a |= 4; 
				reg.a |= 128; // DIP7 Coin info displayed in demo screen. (Active low)
				break;

		case 3: reg.a = shift >> (8 - shiftOffset); break;

	} 
}

void Out(uint8_t value) {

	switch(value) {
		
		case 2: shiftOffset = reg.a & 7; break;
		case 4: shift = reg.a << 8 | (shift & 0x00FF); break;

	}

}

void writeToMem(uint16_t address, uint8_t value) {

	if (address > 0x4000) memory[address - 0x2000] = value;
	else if (address > 0x2000) memory[address] = value;

}

int readFromMem(uint16_t address) {

	uint8_t value = 0;
	
	if (address > 0x4000) memory[address - 0x2000] = value;
	else value = memory[address];

	return value;

}

void drawScreen() { 

	    // the screen is 256 * 224 pixels, and 1 byte contains 8 pixels
    for (int i = 0; i < 256 * 224 / 8; i++) {
        const int y = i * 8 / 256;
        const int base_x = (i * 8) % 256;
        const uint8_t cur_byte = memory[0x2400 + i];

        for (uint8_t bit = 0; bit < 8; bit++) {
            int px = base_x + bit;
            int py = y;
            const bool is_pixel_lit = (cur_byte >> bit) & 1;
            uint8_t r = 0, g = 0, b = 0;

            // colour handling:
            if (!colour && is_pixel_lit) {
                r = 255; g = 255; b = 255;
            }
            else if (colour && is_pixel_lit) {
                if (px < 16) {
                    if (py < 16 || py > 118 + 16) {
                        r = 255; g = 255; b = 255;
                    }
                    else {
                        g = 255;
                    }
                }
                else if (px >= 16 && px <= 16 + 56) {
                    g = 255;
                }
                else if (px >= 16 + 56 + 120 && px < 16 + 56 + 120 + 32) {
                    r = 255;
                }
                else {
                    r = 255; g = 255; b = 255;
                }
            }

            // space invaders' screen is rotated 90 degrees anti-clockwise
            // so we invert the coordinates:
            const int temp_x = px;
            px = py;
            py = -temp_x + 256 - 1;

            screenBuffer[py][px][0] = r;
            screenBuffer[py][px][1] = g;
            screenBuffer[py][px][2] = b;
        }
    }

    const uint32_t pitch = sizeof(uint8_t) * 4 * 224;
    SDL_UpdateTexture(texture, NULL, screenBuffer, pitch);

    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);

}

void dumpVRAM() {

	for (int i=0x8000; i<0x9FFF; i++) {
		std::cout << std::hex << std::uppercase << (int)memory[i];
	}

	std::cout << std::endl;

}

void pushSP(uint16_t value) {

	memory[sp-2] = value << 8;
	memory[sp-1] = value & 0x00FF;

}

void popSP(uint8_t value) {

	switch(value) {

		case 0: reg.c=memory[sp]; reg.b=memory[sp+1]; sp+=2; pc++; break; // POP B
		case 1: reg.e=memory[sp]; reg.d=memory[sp+1]; sp+=2; pc++; break; // POP D
		case 2: reg.l=memory[sp]; reg.h=memory[sp+1]; sp+=2; pc++; break; // POP H

	}
}

void run(int cycles) {

	while (cycles > 0) {
	
	nextByte = memory[pc+1];
	nextWord = (memory[pc+2] << 8) | memory[pc+1];
	
	opcode=memory[pc];

	cycles -= opcodeCycles[opcode];

	if (debug) {
		
		std::cout << std::hex << std::uppercase << std::setfill('0') << std::setw(4) << (int)pc << " " << opcodeTable[opcode] << " cycles remaining: " << std::dec << (int)cycles << std::endl;

	}
	       
	switch(opcode) {

		case 0x00: pc++; break; // NOP

                case 0x01: reg.bc=nextWord; pc+=3; break; // LXI D, D16
                case 0x11: reg.de=nextWord; pc+=3; break; // LXI D, D16
                case 0x21: reg.hl=nextWord; pc+=3; break; // LXI D, D16
                case 0x31: sp=nextWord; pc+=3; break; // LXI SP, D16
		
                case 0x0A: reg.a=readFromMem(reg.bc); pc++; break; // LDAX B
                case 0x1A: reg.a=readFromMem(reg.de); pc++; break; // LDAX D
                case 0x2A: reg.h=readFromMem(nextWord+1); reg.l=readFromMem(nextWord); pc+=3; break; // LHLD D16
                case 0x3A: reg.a=readFromMem(nextWord); pc+=3; break; // LDA #
		
		case 0x02: writeToMem(reg.bc, reg.a); pc++; break; // STAX B
		case 0x12: writeToMem(reg.de, reg.a); pc++; break; // STAX D
                case 0x22: writeToMem(nextWord+1, reg.h); writeToMem(nextWord, reg.l); pc+=3; break; // SHLD D16
                case 0x32: writeToMem(nextWord, reg.a); pc+=3; break; // STA #

                case 0x03: reg.bc++; pc++; break; // INX B
                case 0x13: reg.de++; pc++; break; // INX D
                case 0x23: reg.hl++; pc++; break; // INX H
                case 0x33: sp++; pc++; break; // INX SP

                case 0x0B: reg.bc--; pc++; break; // DCX B
                case 0x1B: reg.de--; pc++; break; // DCX D
                case 0x2B: reg.hl--; pc++; break; // DCX H
                case 0x3B: sp--; pc++; break; // DCX SP

		case 0x04: reg.b++; setZSP(reg.b); pc++; break; // INR B
		case 0x0C: reg.c++; setZSP(reg.c); pc++; break; // INR C
		case 0x14: reg.d++; setZSP(reg.d); pc++; break; // INR D
		case 0x1C: reg.e++; setZSP(reg.e); pc++; break; // INR E
		case 0x24: reg.h++; setZSP(reg.h); pc++; break; // INR H
		case 0x2C: reg.l++; setZSP(reg.l); pc++; break; // INR L	   
		case 0x34: memory[reg.hl]++; setZSP(memory[reg.hl]); pc++; break; // INR M   
		case 0x3C: reg.a++; setZSP(reg.a); pc++; break; // INR A

                case 0x05: reg.b--; setZSP(reg.b); pc++; break; // DCR B
                case 0x0D: reg.c--; setZSP(reg.c); pc++; break; // DCR C
                case 0x15: reg.d--; setZSP(reg.d); pc++; break; // DCR D
		case 0x1D: reg.e--; setZSP(reg.e); pc++; break; // DCR E
                case 0x25: reg.h--; setZSP(reg.h); pc++; break; // DCR H
		case 0x2D: reg.l--; setZSP(reg.l); pc++; break; // DCR L
                case 0x35: memory[reg.hl]--; setZSP(memory[reg.hl]); pc++; break; // DCR M
                case 0x3D: reg.a--; setZSP(reg.a); pc++; break; // DCR A

                case 0x06: reg.b = nextByte; pc+=2; break; // MVI B, D8
                case 0x0E: reg.c = nextByte; pc+=2; break; // MVI C, D8
                case 0x16: reg.d = nextByte; pc+=2; break; // MVI D, D8
                case 0x1E: reg.e = nextByte; pc+=2; break; // MVI E, D8
                case 0x26: reg.h = nextByte; pc+=2; break; // MVI H, D8
                case 0x2E: reg.l = nextByte; pc+=2; break; // MVI L, D8
                case 0x36: writeToMem(reg.hl, nextByte); pc+=2; break; // MVI M, D8
                case 0x3E: reg.a = nextByte; pc+=2; break; // MVI A, D8
		
		case 0x07: flag.c = reg.a >> 7; reg.a = reg.a << 1 | flag.c; pc++; break; // RLC
		case 0x0F: flag.c = reg.a & 1; reg.a = reg.a >> 1 | flag.c << 7; pc++; break; // RRC
		
		case 0x17: flag.c = reg.a >> 7; reg.a = reg.a << 1 | flag.c; pc++; break; // RAL
		case 0x1F: flag.c = reg.a & 1; reg.a = reg.a >> 1 | flag.c >> 7; pc++; break; // RAR
		
		case 0x09: temp32 = reg.hl + reg.bc; if (temp32 & 0xFFFF0000) flag.c=1; else flag.c=0; reg.hl += reg.bc; pc++; break; // DAD B 
		case 0x19: temp32 = reg.hl + reg.de; if (temp32 & 0xFFFF0000) flag.c=1; else flag.c=0; reg.hl += reg.de; pc++; break; // DAD D
		case 0x29: temp32 = reg.hl + reg.hl; if (temp32 & 0xFFFF0000) flag.c=1; else flag.c=0; reg.hl += reg.hl; pc++; break; // DAD H

		case 0x27: pc++; break; // DAA - NOT IMPLEMENTED
		
		case 0x2F: reg.a = ~reg.a; pc++; break; // CMA
		case 0x3F: flag.c = !flag.c; pc++; break; // CMC
		
		case 0x37: flag.c=1; pc++; break; // STC
		
		case 0x40: reg.b=reg.b; pc++; break; // MOV B, B
		case 0x41: reg.b=reg.c; pc++; break; // MOV B, C
		case 0x42: reg.b=reg.d; pc++; break; // MOV B, D
		case 0x43: reg.b=reg.e; pc++; break; // MOV B, E
		case 0x44: reg.b=reg.h; pc++; break; // MOV B, H
		case 0x45: reg.b=reg.l; pc++; break; // MOV B. L
		case 0x46: reg.b=memory[reg.hl]; pc++; break; // MOV B, M
		case 0x47: reg.b=reg.a; pc++; break; // MOV B, A
		
		case 0x48: reg.c=reg.b; pc++; break; // MOV C, B
		case 0x49: reg.c=reg.c; pc++; break; // MOV C, C
		case 0x4A: reg.c=reg.d; pc++; break; // MOV C, D
                case 0x4B: reg.c=reg.e; pc++; break; // MOV C, E
		case 0x4C: reg.c=reg.h; pc++; break; // MOV C, H
                case 0x4D: reg.c=reg.l; pc++; break; // MOV C, L
		case 0x4E: reg.c=memory[reg.hl]; pc++; break; // MOV C, M
		case 0x4F: reg.c=reg.a; pc++; break; // MOV C, A

		case 0x50: reg.d=reg.b; pc++; break; // MOV D, B
		case 0x51: reg.d=reg.c; pc++; break; // MOV D, C
		case 0x52: reg.d=reg.d; pc++; break; // MOV D, D
		case 0x53: reg.d=reg.e; pc++; break; // MOV D, E
		case 0x54: reg.d=reg.h; pc++; break; // MOV D, H
		case 0x55: reg.d=reg.l; pc++; break; // MOV D, L
		case 0x56: reg.d=memory[reg.hl]; pc++; break; // MOV D, M
		case 0x57: reg.d=reg.a; pc++; break; // MOV D, A
		
		case 0x58: reg.e=reg.b; pc++; break; // MOV E, B
		case 0x59: reg.e=reg.c; pc++; break; // MOV E, C
		case 0x5A: reg.e=reg.d; pc++; break; // MOV E, D
		case 0x5B: reg.e=reg.e; pc++; break; // MOV E, E
		case 0x5C: reg.e=reg.h; pc++; break; // MOV E, H
		case 0x5D: reg.e=reg.l; pc++; break; // MOV E, L
		case 0x5E: reg.e=memory[reg.hl]; pc++; break; // MOV E, M
		case 0x5F: reg.e=reg.a; pc++; break; // MOV E, A

		case 0x60: reg.h=reg.b; pc++; break; // MOV H, B
		case 0x61: reg.h=reg.c; pc++; break; // MOV H, C
		case 0x62: reg.h=reg.d; pc++; break; // MOV H, D
		case 0x63: reg.h=reg.e; pc++; break; // MOV H, E
		case 0x64: reg.h=reg.h; pc++; break; // MOV H, H
		case 0x65: reg.h=reg.l; pc++; break; // MOV H, L
		case 0x66: reg.h=memory[reg.hl]; pc++; break; // MOV H, M
		case 0x67: reg.h=reg.a; pc++; break; // MOV H, A

		case 0x68: reg.l=reg.b; pc++; break; // MOV L, B
		case 0x69: reg.l=reg.c; pc++; break; // MOV L, C
		case 0x6A: reg.l=reg.d; pc++; break; // MOV L, D
		case 0x6B: reg.l=reg.e; pc++; break; // MOV L, E
		case 0x6C: reg.l=reg.h; pc++; break; // MOV L, H
		case 0x6D: reg.l=reg.l; pc++; break; // MOV L, L
		case 0x6E: reg.l=memory[reg.hl]; pc++; break; // MOV L, M
		case 0x6F: reg.l=reg.a; pc++; break; // MOV L, A

		case 0x70: writeToMem(reg.hl, reg.b); pc++; break; // MOV M, B
		case 0x71: writeToMem(reg.hl, reg.c); pc++; break; // MOV M, C
		case 0x72: writeToMem(reg.hl, reg.d); pc++; break; // MOV M, D
		case 0x73: writeToMem(reg.hl, reg.e); pc++; break; // MOV M, E
		case 0x74: writeToMem(reg.hl, reg.h); pc++; break; // MOV M, H
		case 0x75: writeToMem(reg.hl, reg.l); pc++; break; // MOV M, L
		case 0x76: writeToMem(reg.hl, reg.hl); pc++; break; // MOV M, M
		case 0x77: writeToMem(reg.hl, reg.a); pc++; break; // MOV M, A

		case 0x78: reg.a=reg.b; pc++; break; // MOV A, B
		case 0x79: reg.a=reg.c; pc++; break; // MOV A, C
		case 0x7A: reg.a=reg.d; pc++; break; // MOV A, D
		case 0x7B: reg.a=reg.e; pc++; break; // MOV A, E
		case 0x7C: reg.a=reg.h; pc++; break; // MOV A, H
		case 0x7D: reg.a=reg.l; pc++; break; // MOV A, L
		case 0x7E: reg.a=memory[reg.hl]; pc++; break; // MOV A, M

		case 0x80: temp16=reg.a+reg.b; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD B
		case 0x81: temp16=reg.a+reg.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD C
		case 0x82: temp16=reg.a+reg.d; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD D	   
                case 0x83: temp16=reg.a+reg.e; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD E
		case 0x84: temp16=reg.a+reg.h; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD H
                case 0x85: temp16=reg.a+reg.l; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD L
                case 0x86: temp16=reg.a+memory[reg.hl]; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD M
		case 0x87: temp16=reg.a+reg.a; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADD A

		case 0x88: temp16=reg.a + reg.b + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC B
		case 0x89: temp16=reg.a + reg.c + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC C
		case 0x8A: temp16=reg.a + reg.d + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC D
		case 0x8B: temp16=reg.a + reg.e + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC E
		case 0x8C: temp16=reg.a + reg.h + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC H
		case 0x8D: temp16=reg.a + reg.l + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC L
		case 0x8E: temp16=reg.a + memory[reg.hl] + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC M
		case 0x8F: temp16=reg.a + reg.a + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // ADC A

		case 0x90: temp16=reg.a - reg.b; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB B
		case 0x91: temp16=reg.a - reg.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB C
		case 0x92: temp16=reg.a - reg.d; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB D
		case 0x93: temp16=reg.a - reg.e; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB E
		case 0x94: temp16=reg.a - reg.h; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB H
		case 0x95: temp16=reg.a - reg.l; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB L
		case 0x96: temp16=reg.a - memory[reg.hl]; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB M
		case 0x97: temp16=reg.a - reg.a; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SUB A

                case 0x98: temp16=reg.a - reg.b - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB B
                case 0x99: temp16=reg.a - reg.c - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB C
                case 0x9A: temp16=reg.a - reg.d - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB D
                case 0x9B: temp16=reg.a - reg.e - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB E
                case 0x9C: temp16=reg.a - reg.h - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB H
                case 0x9D: temp16=reg.a - reg.l - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB L
                case 0x9E: temp16=reg.a - memory[reg.hl] - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB M
                case 0x9F: temp16=reg.a - reg.a - flag.c; if (((temp16 & 0x00FF) >= reg.a) && reg.a) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc++; break; // SBB A

		case 0xA0: reg.a &= reg.b; flag.c=0; setZSP(reg.a); pc++; break; // ANA B
                case 0xA1: reg.a &= reg.c; flag.c=0; setZSP(reg.a); pc++; break; // ANA C
                case 0xA2: reg.a &= reg.d; flag.c=0; setZSP(reg.a); pc++; break; // ANA D
                case 0xA3: reg.a &= reg.e; flag.c=0; setZSP(reg.a); pc++; break; // ANA E
                case 0xA4: reg.a &= reg.h; flag.c=0; setZSP(reg.a); pc++; break; // ANA H
                case 0xA5: reg.a &= reg.l; flag.c=0; setZSP(reg.a); pc++; break; // ANA L
		case 0xA6: reg.a &= memory[reg.hl]; flag.c=0; setZSP(reg.a); pc++; break; // ANA M
		case 0xA7: reg.a &= reg.a; flag.c=0; setZSP(reg.a); pc++; break; // ANA A

		case 0xA8: reg.a ^= reg.b; flag.c=0; setZSP(reg.a); pc++; break; // XRA B
                case 0xA9: reg.a ^= reg.c; flag.c=0; setZSP(reg.a); pc++; break; // XRA C
                case 0xAA: reg.a ^= reg.d; flag.c=0; setZSP(reg.a); pc++; break; // XRA D
                case 0xAB: reg.a ^= reg.e; flag.c=0; setZSP(reg.a); pc++; break; // XRA E
		case 0xAC: reg.a ^= reg.h; flag.c=0; setZSP(reg.a); pc++; break; // XRA H
                case 0xAD: reg.a ^= reg.l; flag.c=0; setZSP(reg.a); pc++; break; // XRA L 
		case 0xAE: reg.a ^= memory[reg.hl]; flag.c=0; setZSP(reg.a); pc++; break; // XRA M	   
		case 0xAF: reg.a ^= reg.a; flag.c=0; setZSP(reg.a); pc++; break; // XRA A

		case 0xB0: reg.a |= reg.b; flag.c=0; setZSP(reg.a); pc++; break; // ORA B
                case 0xB1: reg.a |= reg.c; flag.c=0; setZSP(reg.a); pc++; break; // ORA C
                case 0xB2: reg.a |= reg.d; flag.c=0; setZSP(reg.a); pc++; break; // ORA D
                case 0xB3: reg.a |= reg.e; flag.c=0; setZSP(reg.a); pc++; break; // ORA E
		case 0xB4: reg.a |= reg.h; flag.c=0; setZSP(reg.a); pc++; break; // ORA H
                case 0xB5: reg.a |= reg.l; flag.c=0; setZSP(reg.a); pc++; break; // ORA B
		case 0xB6: reg.a |= memory[reg.hl]; flag.c=0; setZSP(reg.a); pc++; break; // ORA M
		case 0xB7: reg.a |= reg.a; flag.c=0; setZSP(reg.a); pc++; break; // ORA A

		case 0xEE: reg.a ^= nextByte; flag.c=0; setZSP(reg.a); pc+=2; break; // XRI

		case 0xB8: temp16 = reg.a - reg.b; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP B
		case 0xB9: temp16 = reg.a - reg.c; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP C
                case 0xBA: temp16 = reg.a - reg.d; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP D
                case 0xBB: temp16 = reg.a - reg.e; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP E
		case 0xBC: temp16 = reg.a - reg.h; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP H
                case 0xBD: temp16 = reg.a - reg.l; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP L
		case 0xBE: temp16 = reg.a - memory[reg.hl]; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP M
                case 0xBF: temp16 = reg.a - reg.a; flag.c = temp16 >> 8; setZSP(temp16); pc++; break; // CMP C

		case 0xC5: memory[sp-2] = reg.c; memory[sp-1] = reg.b; sp-=2; pc++; break; // PUSH B
		case 0xD5: memory[sp-2] = reg.e; memory[sp-1] = reg.d; sp-=2; pc++; break; // PUSH D
		case 0xE5: memory[sp-2] = reg.l; memory[sp-1] = reg.h; sp-=2; pc++; break; // PUSH H
		case 0xF5: memory[sp-2] = flags; memory[sp-1] = reg.a; sp-=2; pc++; break; // PUSH PSW

		case 0xC1: reg.c=memory[sp]; reg.b=memory[sp+1]; sp+=2; pc++; break; // POP B
		case 0xD1: reg.e=memory[sp]; reg.d=memory[sp+1]; sp+=2; pc++; break; // POP D	   
		case 0xE1: reg.l=memory[sp]; reg.h=memory[sp+1]; sp+=2; pc++; break; // POP H
		case 0xF1: flags = memory[sp]; reg.a=memory[sp+1]; sp+=2; pc++; break; // POP PSW

		case 0xC6: reg.a += nextByte; setZSP(reg.a); pc+=2; break; // ADI
		//case 0xC6: temp16 = reg.a + nextByte; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc+=2; break; // ADI
		case 0xCE: temp16 = reg.a + nextByte + flag.c; if (temp16 & 0xFF00) flag.c=1; else flag.c=0; setZSP(temp16); reg.a=temp16; pc+=2; break; // ACI
		
		case 0xC4: if (!flag.z) { Call(nextWord); cycles-=6; } else pc+=3; break; // CNZ
		case 0xCC: if (flag.z) { Call(nextWord); cycles-=6; } else pc+=3; break; // CZ
		case 0xD4: if (!flag.c) { Call(nextWord); cycles-=6; } else pc+=3; break; // CNC
		case 0xDC: if (flag.c) { Call(nextWord); cycles-=6; } else pc+=3; break; // CNC
		case 0xE4: if (!flag.p) { Call(nextWord); cycles-=6; } else pc+=3; break; // CPO
		case 0xEC: if (flag.p) { Call(nextWord); cycles-=6; } else pc+=3; break; // CPE
		case 0xF4: if (!flag.s) { Call(nextWord); cycles-=6; } else pc+=3; break; // CP
		case 0xFC: if (flag.s) { Call(nextWord); cycles-=6; } else pc+=3; break; // CM
		
		case 0xC0: if (!flag.z) { Return(); cycles-=6; } else pc++; break; // RNZ
		case 0xC8: if (flag.z) { Return(); cycles-=6; } else pc++; break; // RZ
		case 0xD0: if (!flag.c) { Return(); cycles-=6; } else pc++; break; // RNC			   
		case 0xD8: if (flag.c) { Return(); cycles-=6; } else pc++; break; // RC
		case 0xE0: if (!flag.p) { Return(); cycles-=6; } else pc++; break; // RPO
		case 0xE8: if (flag.p) { Return(); cycles-=6; } else pc++; break; // RPE
		case 0xF0: if (!flag.s) { Return(); cycles-=6; } else pc++; break; // RP
		case 0xF8: if (flag.s) { Return(); cycles-=6; } else pc++; break; // RM

		case 0xC9: Return(); break; // RET
		case 0xCD: Call(nextWord); break; // CALL

		case 0xDE: temp16 = reg.a - nextByte - flag.c; if (((temp16 & 0x00FF) >= reg.a) && (nextByte | flag.c)) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc+=2; break; // SBI
		
		case 0xE9: pc=reg.hl; break; // PCHL
		case 0xF9: sp=reg.hl; pc++; break; // SPHL

		case 0xD3: Out(nextByte); pc+=2; break; // OUT
		case 0xDB: In(nextByte); pc+=2; break; // IN

		case 0xC2: if (!flag.z) pc=nextWord; else pc+=3; break; // JNZ
                case 0xC3: pc=nextWord; break; // JMP
		case 0xCA: if (flag.z) pc=nextWord; else pc+=3; break; // JZ
		case 0xD2: if (!flag.c) pc=nextWord; else pc+=3; break; // JNC
		case 0xDA: if (flag.c) pc=nextWord; else pc+=3; break; // JC
		case 0xE2: if (!flag.p) pc=nextWord; else pc+=3; break; // JPO
		case 0xEA: if (flag.p) pc=nextWord; else pc+=3; break; // JPE
		case 0xF2: if (!flag.s) pc=nextWord; else pc+=3; break; // JP
		case 0xFA: if (flag.s) pc=nextWord; else pc+=3; break; // JM

		//case 0xC7: case 0xCF: case 0xD7: case 0xDF: case 0xE7: case 0xEF: case 0xF7: case 0xFF:

			//pc=Rst(nextByte); break;
			
		case 0xC7: pc=0x0; break; // RST 0
		case 0xD7: pc=0x10; break; // RST 2
		case 0xDF: pc=0x18; break; // RST 3
		case 0xFF: pc=0x38; break; // RST 7
		
		case 0xD6: temp16 = reg.a - nextByte; if (((temp16 & 0x00FF) >= reg.a) && nextByte) flag.c=1; else flag.c=0; reg.a=temp16; setZSP(reg.a); pc+=2; break; // SUI 
                case 0xE6: reg.a &= nextByte; flag.c=0; setZSP(reg.a); pc+=2; break; // ANI
		case 0xF6: reg.a |= nextByte; flag.c=0; setZSP(reg.a); pc+=2; break; // ORI
		
		case 0xE3: { // XTHL
				   
				temp16 = memory[sp+1] << 8 | (memory[sp] & 0xFF);
				memory[sp+1] = reg.h;
				memory[sp] = reg.l;
				reg.hl = temp16;
				pc++;
				break;

		}
		
		case 0xEB: { // XCHG

				temp16=reg.de; reg.de=reg.hl; reg.hl=temp16;
				pc++;
				break;

		}

		case 0xF3: iff=0; pc++; break; // DI
		case 0xFB: iff=1; pc++; break; // EI
		
		case 0xFE: s16 = reg.a - nextByte; flag.c = (s16 & 0b100000000) != 0; setZSP(s16); pc+=2; break; // CPI

		default: std::cout << std::hex << "Unimplemented opcode: " << (int)opcode << std::endl; exit(0);

	}

if (debug) dumpRegs();
//readSerial();

}
}

int main() {

	init();

        while(true) { 
	
		run(2000000/30);
		generateInterrupt(1);
		run(2000000/30);
		generateInterrupt(2);

      if (SDL_GetTicks() - timer > (1.0f / 60.0f) * 1000) {	

		      drawScreen();
			
			SDL_PollEvent(&event);
			if (event.type == SDL_QUIT || event.key.keysym.sym == SDLK_q) exit(0);
			timer=SDL_GetTicks();

	}
	}

//      dumpVRAM();

}
