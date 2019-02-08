-- This script runs the ZEXDOC test through the emulator.

if not bit then bit = bit32 end

dofile("Z80.lua")

m = Memory()
x = Z80(m)

local clock = os.clock
function sleep(n) -- seconds
    local t0 = clock()
    while clock() - t0 <= n do
    end
end

function loadBin(path, offset)
    local count = 0
    local input = io.open(path, "rb")
    while true do
        local x = input:read(1)
        if x == nil then
            break
        end
        m:mem_write(count + offset, string.byte(x))
        count = count + 1
    end
end

loadBin("zexdoc.ams", 0x100)

m:mem_write(0, 0xC3)
m:mem_write(1, 0x00)
m:mem_write(2, 0x01)
m:mem_write(5, 0xC9)

running = true

while running do  
    for i = 1, 750000 do

        x:run_instruction()

        if (x.pc == 0x05) then

            if (x.c == 0) then
                print("Z80 reset after " .. x.cycle_counter .. " t-states")
                running = false
                break
            end

            if (x.c == 2) then
                io.write(string.format("%2x", x.e))
            end

            if (x.c == 9) then
                local de = bit.bor(bit.lshift(x.d, 8), x.e)
                local strAddr = de
                while (string.char(m:mem_read(strAddr)) ~= "$") do
                    io.write(string.char(m:mem_read(strAddr)))
                    strAddr = strAddr + 1
                end
            end
        end
    end

    io.flush()
    sleep(0.05)
end

