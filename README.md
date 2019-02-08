# LuaZ80

![alt text](https://i.imgur.com/Q8qLcOJ.png)

This is a Z80 emulator core written in pure Lua, meant to be used as a base for other emulator projects. 

It is fully compliant with, and can pass all ZEXDOC tests with no errors and is recommended to be ran with LuaJIT to get reasonable performance.

This emulator was inspired by [DrGoldfire's](https://github.com/DrGoldfire/Z80.js?files=1) javascript emulator.




## Usage
The Z80.lua file contains the Z80 code itself as well as a memory "class" that the Z80 needs to operate, however as long as you expose the right methods you can build your own memory class. 

```lua
dofile("Z80.lua")
mem = Memory()
z80 = Z80(mem)
```

These are the methods meant to be used to interact with the Z80.

```lua
-- Resets the processor
reset()

--Runs the instruction pointed to by the PC. Increments the PC
run_instruction()

--Triggers an interrupt
interrupt(non_maskable, data)

--Returns a table populated with the current state of the CPU. Includes flags, registers, PC, etc.
getState()

--Sets the state of the cpu. Should be a table in the same format as returned by getState()
setState(state)

```


## License
[MIT](https://choosealicense.com/licenses/mit/)





(Yes, I know the picture isn't a Z80, I spent entirely too long in photoshop to try again)
