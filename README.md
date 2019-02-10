# LuaZ80

![alt text](https://i.imgur.com/Q8qLcOJ.png)

This is a Z80 emulator core written in pure Lua, meant to be used as a base for other emulator projects. 

It is fully compliant with, and can pass all ZEXDOC tests with no errors and is recommended to be ran with LuaJIT to get reasonable performance.

This emulator was inspired by [DrGoldfire's](https://github.com/DrGoldfire/Z80.js?files=1) javascript emulator.




## Usage
The Z80.lua file contains the Z80 code itself as well as a memory "class" that the Z80 needs to operate, however as long as you provide the right methods you can substitute your own memory class. 

```lua
dofile("Z80.lua")
mem = Memory()
z80 = Z80(mem)
```

These are the methods meant to be used to interact with the Z80.

```lua
-- Resets the processor
z80:reset()

--Runs the instruction pointed to by the PC. Increments the PC
z80:run_instruction()

--Triggers an interrupt (boolean, value)
z80:interrupt(non_maskable, data)

--Returns a table populated with the current state of the CPU. Includes flags, registers, PC, etc.
z80:getState()

--Sets the state of the cpu. Should be a table in the same format as returned by getState()
z80:setState(state)

```

Here are some useful methods for the built-in memory class

```lua
--Initialize a memory bank
mem = Memory()
or
mem = Memory(ramSize, portSize)

-- Set the objects memory table to the provided memory table
mem:setRam(ram)

-- Set the objects ports table to the provided ports table
mem:setPorts()

-- Read the byte value at a given memory address
mem:mem_read(address)

-- Write a byte value at a given memory address
mem:mem_write(address, value)

-- Read the byte value at a given port address
mem:io_read(address)

-- Write a byte value at a given port address
mem:io_write(address, value)

-- Prints a dump of the currently loaded memory
mem:dumpMemory()

```


## License
[MIT](https://choosealicense.com/licenses/mit/)





(Yes, I know the picture isn't a Z80, I spent entirely too long in photoshop to try again)
