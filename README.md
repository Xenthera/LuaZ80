# LuaZ80

This is a Z80 emulator core written in pure Lua, meant to be used as a base for other emulator projects. 

It is fully compliant with, and can pass all ZEXDOC tests with no errors and is recommended to be ran with LuaJIT to get reasonable performance.

This emulator was inspired by [DrGoldfire's](https://github.com/DrGoldfire/Z80.js?files=1) javascript emulator.




## Usage
The Z80.lua file contains the Z80 code itself as well as a memory "class" that the Z80 needs to operate, however as long as you expose the right methods you can build your own class as well. 

```lua
dofile("Z80.lua")
mem = Memory()
z80 = Z80(mem)
```



## License
[MIT](https://choosealicense.com/licenses/mit/)

