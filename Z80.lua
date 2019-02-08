Memory = {}

setmetatable(Memory, {
  __call = function (class, ...)
  	numArgs = select("#", ...)
  	if numArgs == 0 then
  		return class.newArg0(class)
  	elseif numArgs == 2 then
  		return class.newArg2(class, ...)
  	end
  end,
})

Memory.z80Ram = {}
Memory.z80Ports = {}

Memory.newArg0 = function(self)
	for i=0, 0x10000 - 1 do
		self.z80Ram[i] = 0x0;
	end

	for i=0, 0x10000 - 1 do
		self.z80Ports[i] = 0x0;
	end

	return self
end

Memory.newArg2 = function(self, ramSize, portSize)
	if ramSize < 0 or ramSize > 0x10000 then
		error("ramSize Out of Range [0x0000 - 0x10000]")
	end
	if ramSize > 0 then
		for i=0, ramSize - 1 do
			self.z80Ram[i] = 0x0;
		end
	end

	if portSize < 0 or portSize > 0x10000 then
		error("portSize Out of Range [0x0000 - 0x10000]")
	end

	if portSize > 0 then
		for i=0, portSize - 1 do
			self.z80Ports[i] = 0x0;
		end
	end

	return self
end

Memory.setRam = function(self, ram)
	self.z80Ram = ram;
end

Memory.setPorts = function(self, ports)
	self.z80Ports = ports;
end

Memory.mem_read = function(self, address) 
	if address > #self.z80Ram then
		error("mem_read - Address out of bounds: 0x"..string.format("%x", address))
	else
	    return bit.band(self.z80Ram[address], 0xff);
	end
end

Memory.mem_write = function(self, address, value) 
	if address > #self.z80Ram then
		error("mem_write - Address out of bounds: 0x"..string.format("%x", address))
	else

	    rawset(self.z80Ram, address, bit.band(value, 0xFF))
	end
end

Memory.io_read = function(self, port) 
    return bit.band(self.z80Ports[port], 0xff);
end

Memory.io_write = function(self, port, value) 
    self.z80Ports[port] = bit.band(value, 0xFF);
end

Memory.dumpMemory = function(self)
	buf = ""
	for i=0,#self.z80Ram do
		buf = buf .. string.char(self.z80Ram[i])
	end

    for i=1,math.ceil(#buf/16) * 16 do
        if (i-1) % 16 == 0 then io.write(string.format('%08X  ', i-1)) end
        io.write( i > #buf and '   ' or string.format('%02X ', buf:byte(i)) )
        if i %  8 == 0 then io.write(' ') end
        if i % 16 == 0 then io.write( buf:sub(i-16+1, i):gsub('%c','.'), '\n' ) end
    end
end

Z80 = {}

setmetatable(
    Z80,
    {
        __call = function(class, ...)
            return class.new(class, ...)
        end
    }
)

Z80.a = 0
Z80.b = 0
Z80.c = 0
Z80.d = 0
Z80.e = 0
Z80.h = 0
Z80.l = 0
Z80.a_prime = 0
Z80.b_prime = 0
Z80.c_prime = 0
Z80.d_prime = 0
Z80.e_prime = 0
Z80.h_prime = 0
Z80.l_prime = 0
Z80.ix = 0
Z80.iy = 0
Z80.i = 0
Z80.r = 0
Z80.sp = 57328
Z80.pc = 0
Z80.flags = {
    S = false,
    Z = false,
    Y = false,
    H = false,
    X = false,
    P = false,
    N = false,
    C = false
}
Z80.flags_prime = {
    S = false,
    Z = false,
    Y = false,
    H = false,
    X = false,
    P = false,
    N = false,
    C = false
}
Z80.imode = 0
Z80.iff1 = 0
Z80.iff2 = 0
Z80.halted = false
Z80.do_delayed_di = false
Z80.do_delayed_ei = false
Z80.cycle_counter = 0

Z80.new = function(self, memory)
    if
        (((not memory or (type(memory.mem_read) ~= "function")) or (type(memory.mem_write) ~= "function")) or
            (type(memory.io_read) ~= "function")) or
            (type(memory.io_write) ~= "function")
     then
        error("Z80: Core object is missing required functions.")
    end
    self.memory = memory

    return self
end

function Z80.getState(self)
    return {
        b = self.b,
        a = self.a,
        c = self.c,
        d = self.d,
        e = self.e,
        h = self.h,
        l = self.l,
        a_prime = self.a_prime,
        b_prime = self.b_prime,
        c_prime = self.c_prime,
        d_prime = self.d_prime,
        e_prime = self.e_prime,
        h_prime = self.h_prime,
        l_prime = self.l_prime,
        ix = self.ix,
        iy = self.iy,
        i = self.i,
        r = self.r,
        sp = self.sp,
        pc = self.pc,
        flags = {
            S = self.flags.S,
            Z = self.flags.Z,
            Y = self.flags.Y,
            H = self.flags.H,
            X = self.flags.X,
            P = self.flags.P,
            N = self.flags.N,
            C = self.flags.C
        },
        flags_prime = {
            S = self.flags_prime.S,
            Z = self.flags_prime.Z,
            Y = self.flags_prime.Y,
            H = self.flags_prime.H,
            X = self.flags_prime.X,
            P = self.flags_prime.P,
            N = self.flags_prime.N,
            C = self.flags_prime.C
        },
        imode = self.imode,
        iff1 = self.iff1,
        iff2 = self.iff2,
        halted = self.halted,
        do_delayed_di = self.do_delayed_di,
        do_delayed_ei = self.do_delayed_ei,
        cycle_counter = self.cycle_counter
    }
end
function Z80.setState(self, state)
    self.b = state.b
    self.a = state.a
    self.c = state.c
    self.d = state.d
    self.e = state.e
    self.h = state.h
    self.l = state.l
    self.a_prime = state.a_prime
    self.b_prime = state.b_prime
    self.c_prime = state.c_prime
    self.d_prime = state.d_prime
    self.e_prime = state.e_prime
    self.h_prime = state.h_prime
    self.l_prime = state.l_prime
    self.ix = state.ix
    self.iy = state.iy
    self.i = state.i
    self.r = state.r
    self.sp = state.sp
    self.pc = state.pc
    self.flags.S = state.flags.S
    self.flags.Z = state.flags.Z
    self.flags.Y = state.flags.Y
    self.flags.H = state.flags.H
    self.flags.X = state.flags.X
    self.flags.P = state.flags.P
    self.flags.N = state.flags.N
    self.flags.C = state.flags.C
    self.flags_prime.S = state.flags_prime.S
    self.flags_prime.Z = state.flags_prime.Z
    self.flags_prime.Y = state.flags_prime.Y
    self.flags_prime.H = state.flags_prime.H
    self.flags_prime.X = state.flags_prime.X
    self.flags_prime.P = state.flags_prime.P
    self.flags_prime.N = state.flags_prime.N
    self.flags_prime.C = state.flags_prime.C
    self.imode = state.imode
    self.iff1 = state.iff1
    self.iff2 = state.iff2
    self.halted = state.halted
    self.do_delayed_di = state.do_delayed_di
    self.do_delayed_ei = state.do_delayed_ei
    self.cycle_counter = state.cycle_counter
end
Z80.reset = function(self)
    self.sp = 57328
    self.pc = 0
    self.a = 0
    self.r = 0
    self:set_flags_register(0)
    self.imode = 0
    self.iff1 = 0
    self.iff2 = 0
    self.halted = false
    self.do_delayed_di = false
    self.do_delayed_ei = false
    self.cycle_counter = 0
end
Z80.run_instruction = function(self)
    if not self.halted then
        local doing_delayed_di, doing_delayed_ei = false, false
        if self.do_delayed_di then
            self.do_delayed_di = false
            doing_delayed_di = true
        elseif self.do_delayed_ei then
            self.do_delayed_ei = false
            doing_delayed_ei = true
        end
        self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
        local opcode = self.memory.mem_read(self.memory, self.pc)
        self:decode_instruction(opcode)
        self.pc = bit.band((self.pc + 1), 65535)
        if doing_delayed_di then
            self.iff1 = 0
            self.iff2 = 0
        elseif doing_delayed_ei then
            self.iff1 = 1
            self.iff2 = 1
        end
        local retval = self.cycle_counter
        return retval
    else
        return 1
    end
end
Z80.interrupt = function(self, non_maskable, data)
    if non_maskable then
        self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
        self.halted = false
        self.iff2 = self.iff1
        self.iff1 = 0
        self:push_word(self.pc)
        self.pc = 102
        self.cycle_counter = self.cycle_counter + 11
    elseif self.iff1 then
        self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
        self.halted = false
        self.iff1 = 0
        self.iff2 = 0
        if self.imode == 0 then
            self:decode_instruction(data)
            self.cycle_counter = self.cycle_counter + 2
        elseif self.imode == 1 then
            self:push_word(self.pc)
            self.pc = 56
            self.cycle_counter = self.cycle_counter + 13
        elseif self.imode == 2 then
            self:push_word(self.pc)
            local vector_address = bit.bor(bit.lshift(i, 8), data)
            self.pc =
                bit.bor(
                self.memory.mem_read(self.memory, vector_address),
                bit.lshift(self.memory.mem_read(self.memory, bit.band((vector_address + 1), 65535)), 8)
            )
            self.cycle_counter = self.cycle_counter + 19
        end
    end
end
Z80.decode_instruction = function(self, opcode)
    local get_operand = function(opcode)
        return (bit.band(opcode, 7) == 0) and self.b or (bit.band(opcode, 7) == 1) and self.c or
            (bit.band(opcode, 7) == 2) and self.d or
            (bit.band(opcode, 7) == 3) and self.e or
            (bit.band(opcode, 7) == 4) and self.h or
            (bit.band(opcode, 7) == 5) and self.l or
            (bit.band(opcode, 7) == 6) and self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))) or
            self.a
    end
    if opcode == 118 then
        self.halted = true
    elseif (opcode >= 64) and (opcode < 128) then
        local operand = get_operand(opcode)
        if bit.rshift(bit.band(opcode, 56), 3) == 0 then
            self.b = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 1 then
            self.c = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 2 then
            self.d = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 3 then
            self.e = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 4 then
            self.h = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 5 then
            self.l = operand
        elseif bit.rshift(bit.band(opcode, 56), 3) == 6 then
            self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), operand)
        elseif bit.rshift(bit.band(opcode, 56), 3) == 7 then
            self.a = operand
        end
    elseif (opcode >= 128) and (opcode < 192) then
        local operand, op_array =
            get_operand(opcode),
            {self.do_add, self.do_adc, self.do_sub, self.do_sbc, self.do_and, self.do_xor, self.do_or, self.do_cp}
        op_array[bit.rshift(bit.band(opcode, 56), 3) + 1](self, operand)
    else
        local func = self.instructions[opcode]
        func(self)
    end
    self.cycle_counter = self.cycle_counter + self.cycle_counts[opcode]
end
Z80.get_signed_offset_byte = function(self, value)
    value = bit.band(value, 255)
    if bit.band(value, 128) ~= 0 then
        value = -(bit.band(255, bit.bnot(value)) + 1)
    end
    return value
end
Z80.get_flags_register = function(self)
    local flagS, flagZ, flagY, flagH, flagX, flagP, flagN, flagC = 0, 0, 0, 0, 0, 0, 0, 0
    if (self.flags.S == true) then
        flagS = 1
    else
        flagS = 0
    end
    if (self.flags.Z == true) then
        flagZ = 1
    else
        flagZ = 0
    end
    if (self.flags.Y == true) then
        flagY = 1
    else
        flagY = 0
    end
    if (self.flags.H == true) then
        flagH = 1
    else
        flagH = 0
    end
    if (self.flags.X == true) then
        flagX = 1
    else
        flagX = 0
    end
    if (self.flags.P == true) then
        flagP = 1
    else
        flagP = 0
    end
    if (self.flags.N == true) then
        flagN = 1
    else
        flagN = 0
    end
    if (self.flags.C == true) then
        flagC = 1
    else
        flagC = 0
    end
    local ret =
        bit.bor(
        bit.bor(
            bit.bor(
                bit.bor(
                    bit.bor(bit.bor(bit.bor(bit.lshift(flagS, 7), bit.lshift(flagZ, 6)), bit.lshift(flagY, 5)), bit.lshift(flagH, 4)),
                    bit.lshift(flagX, 3)
                ),
                bit.lshift(flagP, 2)
            ),
            bit.lshift(flagN, 1)
        ),
        flagC
    )
    return ret
end
Z80.get_flags_prime = function(self)

    local flag_primeS, flag_primeZ, flag_primeY, flag_primeH, flag_primeX, flag_primeP, flag_primeN, flag_primeC =
        0, 0, 0, 0, 0, 0, 0, 0
    if (self.flags_prime.S == true) then
        flag_primeS = 1
    else
        flag_primeS = 0
    end
    if (self.flags_prime.Z == true) then
        flag_primeZ = 1
    else
        flag_primeZ = 0
    end
    if (self.flags_prime.Y == true) then
        flag_primeY = 1
    else
        flag_primeY = 0
    end
    if (self.flags_prime.H == true) then
        flag_primeH = 1
    else
        flag_primeH = 0
    end
    if (self.flags_prime.X == true) then
        flag_primeX = 1
    else
        flag_primeX = 0
    end
    if (self.flags_prime.P == true) then
        flag_primeP = 1
    else
        flag_primeP = 0
    end
    if (self.flags_prime.N == true) then
        flag_primeN = 1
    else
        flag_primeN = 0
    end
    if (self.flags_prime.C == true) then
        flag_primeC = 1
    else
        flag_primeC = 0
    end
    local ret =
        bit.bor(
        bit.bor(
            bit.bor(
                bit.bor(
                    bit.bor(
                        bit.bor(bit.bor(bit.lshift(flag_primeS, 7), bit.lshift(flag_primeZ, 6)), bit.lshift(flag_primeY, 5)),
                        bit.lshift(flag_primeH, 4)
                    ),
                    bit.lshift(flag_primeX, 3)
                ),
                bit.lshift(flag_primeP, 2)
            ),
            bit.lshift(flag_primeN, 1)
        ),
        flag_primeC
    )
    return ret
end
Z80.set_flags_register = function(self, operand)
    if (bit.rshift(bit.band(operand, 128), 7)) == 1 then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.rshift(bit.band(operand, 64), 6)) == 1 then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.rshift(bit.band(operand, 32), 5)) == 1 then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band(operand, 16), 4)) == 1 then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if (bit.rshift(bit.band(operand, 8), 3)) == 1 then
        self.flags.X = true
    else
        self.flags.X = false
    end
    if (bit.rshift(bit.band(operand, 4), 2)) == 1 then
        self.flags.P = true
    else
        self.flags.P = false
    end
    if (bit.rshift(bit.band(operand, 2), 1)) == 1 then
        self.flags.N = true
    else
        self.flags.N = false
    end
    if bit.band(operand, 1) == 1 then
        self.flags.C = true
    else
        self.flags.C = false
    end
end
Z80.set_flags_prime = function(self, operand)
    if (bit.rshift(bit.band(operand, 128), 7)) == 1 then
        self.flags_prime.S = true
    else
        self.flags_prime.S = false
    end
    if (bit.rshift(bit.band(operand, 64), 6)) == 1 then
        self.flags_prime.Z = true
    else
        self.flags_prime.Z = false
    end
    if (bit.rshift(bit.band(operand, 32), 5)) == 1 then
        self.flags_prime.Y = true
    else
        self.flags_prime.Y = false
    end
    if (bit.rshift(bit.band(operand, 16), 4)) == 1 then
        self.flags_prime.H = true
    else
        self.flags_prime.H = false
    end
    if (bit.rshift(bit.band(operand, 8), 3)) == 1 then
        self.flags_prime.X = true
    else
        self.flags_prime.X = false
    end
    if (bit.rshift(bit.band(operand, 4), 2)) == 1 then
        self.flags_prime.P = true
    else
        self.flags_prime.P = false
    end
    if (bit.rshift(bit.band(operand, 2), 1)) == 1 then
        self.flags_prime.N = true
    else
        self.flags_prime.N = false
    end
    if bit.band(operand, 1) == 1 then
        self.flags_prime.C = true
    else
        self.flags_prime.C = false
    end
end
Z80.update_xy_flags = function(self, result)
    if (bit.rshift(bit.band(result, 32), 5) > 0) then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band(result, 8), 3) > 0) then
        self.flags.X = true
    else
        self.flags.X = false
    end
end
Z80.get_parity = function(self, value)
    local parity_bits = {
        1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1
    }
    if (parity_bits[value + 1] == 1) then
        return true
    else
        return false
    end
end
Z80.push_word = function(self, operand)
    self.sp = bit.band((self.sp - 1), 65535)
    self.memory.mem_write(self.memory, self.sp, bit.rshift(bit.band(operand, 65280), 8))
    self.sp = bit.band((self.sp - 1), 65535)
    self.memory.mem_write(self.memory, self.sp, bit.band(operand, 255))
end
Z80.pop_word = function(self)
    local retval = bit.band(self.memory.mem_read(self.memory, self.sp), 255)
    self.sp = bit.band((self.sp + 1), 65535)
    retval = bit.bor(retval, bit.lshift(self.memory.mem_read(self.memory, self.sp), 8))
    self.sp = bit.band((self.sp + 1), 65535)
    return retval
end
Z80.do_conditional_absolute_jump = function(self, condition)
    if condition then
        self.pc =
            bit.bor(
            self.memory.mem_read(self.memory, bit.band((self.pc + 1), 0xffff)),
            bit.lshift(self.memory.mem_read(self.memory, bit.band((self.pc + 2), 0xffff)), 8)
        )
        self.pc = bit.band((self.pc - 1), 65535)
    else
        self.pc = bit.band((self.pc + 2), 65535)
    end
end
Z80.do_conditional_relative_jump = function(self, condition)
    if condition then
        self.cycle_counter = self.cycle_counter + 5
        local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)))
        self.pc = bit.band(((self.pc + offset) + 1), 65535)
    else
        self.pc = bit.band((self.pc + 1), 65535)
    end
end
Z80.do_conditional_call = function(self, condition)
    if condition then
        self.cycle_counter = self.cycle_counter + 7
        self:push_word(bit.band((self.pc + 3), 65535))
        self.pc =
            bit.bor(
            self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)),
            bit.lshift(self.memory.mem_read(self.memory, bit.band((self.pc + 2), 65535)), 8)
        )
        self.pc = bit.band((self.pc - 1), 65535)
    else
        self.pc = bit.band((self.pc + 2), 65535)
    end
end
Z80.do_conditional_return = function(self, condition)
    if condition then
        self.cycle_counter = self.cycle_counter + 6
        self.pc = bit.band((self:pop_word() - 1), 65535)
    end
end
Z80.do_reset = function(self, address)
    self:push_word(bit.band((self.pc + 1), 65535))
    self.pc = bit.band((address - 1), 65535)
end
Z80.do_add = function(self, operand)
    local result = self.a + operand
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band((bit.band(operand, 15) + bit.band(self.a, 15)), 16) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((bit.band(self.a, 128) == bit.band(operand, 128)) and (bit.band(self.a, 128) ~= bit.band(result, 128))) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    if (bit.band(result, 256) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.a = bit.band(result, 255)
    self:update_xy_flags(self.a)
end
Z80.do_adc = function(self, operand)
    local flagC = 0
    if (self.flags.C) then
        flagC = 1
    end
    local result = (self.a + operand) + flagC
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band(((bit.band(operand, 15) + bit.band(self.a, 15)) + flagC), 16) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((bit.band(self.a, 128) == bit.band(operand, 128)) and (bit.band(self.a, 128) ~= bit.band(result, 128))) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    if (bit.band(result, 256) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.a = bit.band(result, 255)
    self:update_xy_flags(self.a)
end

Z80.do_sub = function(self, operand)
    local result = self.a - operand
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band((bit.band(self.a, 15) - bit.band(operand, 15)), 16) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((bit.band(self.a, 128) ~= bit.band(operand, 128)) and (bit.band(self.a, 128) ~= bit.band(result, 128))) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = true
    if (bit.band(result, 256) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.a = bit.band(result, 255)
    self:update_xy_flags(self.a)
end
Z80.do_sbc = function(self, operand)
    local flagC = 0
    if (self.flags.C) then
        flagC = 1
    end
    local result = (self.a - operand) - flagC
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band(((bit.band(self.a, 15) - bit.band(operand, 15)) - flagC), 16) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((bit.band(self.a, 128) ~= bit.band(operand, 128)) and (bit.band(self.a, 128) ~= bit.band(result, 128))) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = true
    if (bit.band(result, 256) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.a = bit.band(result, 255)
    self:update_xy_flags(self.a)
end
Z80.do_cp = function(self, operand)
    local temp = self.a
    self:do_sub(operand)
    self.a = temp
    self:update_xy_flags(operand)
end
Z80.do_and = function(self, operand)
    self.a = bit.band(bit.band(self.a, operand), 255)
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    --print(self.flags.Z)
    self.flags.H = true
    self.flags.P = self:get_parity(self.a)
    self.flags.N = false
    self.flags.C = false
    self:update_xy_flags(self.a)
end
Z80.do_or = function(self, operand)
    self.a = bit.band(bit.bor(operand, self.a), 255)

    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.H = false
    self.flags.P = self:get_parity(self.a)
    self.flags.N = false
    self.flags.C = false
    self:update_xy_flags(self.a)
end
Z80.do_xor = function(self, operand)
    self.a = bit.band(bit.bxor(operand, self.a), 255)
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end

    if (self.a == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.H = false
    self.flags.P = self:get_parity(self.a)
    self.flags.N = false
    self.flags.C = false
    self:update_xy_flags(self.a)
end
Z80.do_inc = function(self, operand)
    local result = operand + 1
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if ((bit.band(operand, 15) == 15) == true) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((operand == 127) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    result = bit.band(result, 255)
    self:update_xy_flags(result)
    return result
end
Z80.do_dec = function(self, operand)
    local result = operand - 1
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 255) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if ((bit.band(operand, 15) == 0) == true) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((operand == 128) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = true
    result = bit.band(result, 255)
    self:update_xy_flags(result)
    return result
end
Z80.do_hl_add = function(self, operand)
    local hl = bit.bor(self.l, bit.lshift(self.h, 8))
    local result = hl + operand
    self.flags.N = false
    if (bit.band(result, 65536) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    if (bit.band((bit.band(hl, 4095) + bit.band(operand, 4095)), 4096) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    self:update_xy_flags(self.h)
end
Z80.do_hl_adc = function(self, operand)
    local flagC = 0
    if (self.flags.C) then
        flagC = 1
    end
    operand = operand + flagC
    local hl = bit.bor(self.l, bit.lshift(self.h, 8))
    local result = hl + operand
    if (bit.band(result, 32768) ~= 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 65535) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band((bit.band(hl, 4095) + bit.band(operand, 4095)), 4096) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if (((bit.band(hl, 32768) == bit.band(operand, 32768)) and (bit.band(result, 32768) ~= bit.band(hl, 32768))) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    if (bit.band(result, 65536) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.l = bit.band(result, 255)
    self.h = bit.band(bit.rshift(result, 8), 255)
    self:update_xy_flags(self.h)
end
Z80.do_hl_sbc = function(self, operand)
    --print(operand)
    local flagC = 0
    if (self.flags.C) then
        flagC = 1
    end
    operand = operand + flagC
    local hl = bit.bor(self.l, bit.lshift(self.h, 8))
    local result = hl - operand
    if (bit.band(result, 32768) ~= 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (bit.band(result, 65535) == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.band((bit.band(hl, 4095) - bit.band(operand, 4095)), 4096) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if (((bit.band(hl, 32768) ~= bit.band(operand, 32768)) and (bit.band(result, 32768) ~= bit.band(hl, 32768))) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = true
    if (bit.band(result, 65536) > 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.l = bit.band(result, 255)
    self.h = bit.band(bit.rshift(result, 8), 255)
    self:update_xy_flags(self.h)
end
Z80.do_in = function(self, port)
    local result = self.memory.io_read(self.memory, port)
    if (bit.band(result, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (result ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.H = false
    if (self:get_parity(result) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    self:update_xy_flags(result)
    return result
end
Z80.do_neg = function(self)
    if self.a ~= 128 then
        self.a = self:get_signed_offset_byte(self.a)
        self.a = bit.band(-self.a, 255)
    end
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a == 0) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if ((bit.band(-self.a, 15) > 0) == true) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    if ((self.a == 128) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = true
    if (self.a ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self:update_xy_flags(self.a)
end
Z80.do_ldi = function(self)
    local read_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    self.memory.mem_write(self.memory, bit.bor(self.e, bit.lshift(self.d, 8)), read_value)
    local result = bit.bor(self.e, bit.lshift(self.d, 8)) + 1
    self.e = bit.band(result, 255)
    self.d = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.l, bit.lshift(self.h, 8)) + 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.c, bit.lshift(self.b, 8)) - 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
    self.flags.H = false
    if ((self.c or self.b) ~= 0) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    if (bit.rshift(bit.band((self.a + read_value), 2), 1) == 1) then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band((self.a + read_value), 8), 3) == 1) then
        self.flags.X = true
    else
        self.flags.X = false
    end
end
Z80.do_cpi = function(self)
    local temp_carry = self.flags.C
    local read_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    self:do_cp(read_value)
    self.flags.C = temp_carry
    local flagH = 0
    if self.flags.H then
        flagH = 1
    end
    if (bit.rshift(bit.band(((self.a - read_value) - flagH), 2), 1) == 1) then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band(((self.a - read_value) - flagH), 8), 3) == 1) then
        self.flags.X = true
    else
        self.flags.X = false
    end
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) + 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.c, bit.lshift(self.b, 8)) - 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
    if (result ~= 0) then
        self.flags.P = true
    else
        self.flags.P = false
    end
end
Z80.do_ini = function(self)
    self.b = self:do_dec(self.b)
    self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), self.memory.io_read(self.memory, bit.bor(bit.lshift(self.b, 8), self.c)))
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) + 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    self.flags.N = true
end
Z80.do_outi = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))))
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) + 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    self.b = self:do_dec(self.b)
    self.flags.N = true
end
Z80.do_ldd = function(self)
    self.flags.N = false
    self.flags.H = false
    local read_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    self.memory.mem_write(self.memory, bit.bor(self.e, bit.lshift(self.d, 8)), read_value)
    local result = bit.bor(self.e, bit.lshift(self.d, 8)) - 1
    self.e = bit.band(result, 255)
    self.d = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.l, bit.lshift(self.h, 8)) - 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.c, bit.lshift(self.b, 8)) - 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
    if ((self.c ~= 0) or (self.b ~= 0)) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    if (bit.rshift(bit.band((self.a + read_value), 2), 1) == 1) then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band((self.a + read_value), 8), 3) == 1) then
        self.flags.X = true
    else
        self.flags.X = false
    end
end
Z80.do_cpd = function(self)
    local temp_carry = self.flags.C
    local read_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    self:do_cp(read_value)
    self.flags.C = temp_carry
    local flagH = 0
    if self.flags.H then
        flagH = 1
    end
    if (bit.rshift(bit.band(((self.a - read_value) - flagH), 2), 1) == 1) then
        self.flags.Y = true
    else
        self.flags.Y = false
    end
    if (bit.rshift(bit.band(((self.a - read_value) - flagH), 8), 3) == 1) then
        self.flags.X = true
    else
        self.flags.X = false
    end
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) - 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    result = bit.bor(self.c, bit.lshift(self.b, 8)) - 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
    if (result ~= 0) then
        self.flags.P = true
    else
        self.flags.P = false
    end
end
Z80.do_ind = function(self)
    self.b = self:do_dec(self.b)
    self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), self.memory.io_read(self.memory, bit.bor(bit.lshift(self.b, 8), self.c)))
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) - 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    self.flags.N = true
end
Z80.do_outd = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))))
    local result = bit.bor(self.l, bit.lshift(self.h, 8)) - 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
    self.b = self:do_dec(self.b)
    self.flags.N = true
end
Z80.do_rlc = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.rshift(bit.band(operand, 128), 7) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    local flagC = 0
    if self.flags.C == true then
        flagC = 1
    end
    operand = bit.band(bit.bor(bit.lshift(operand, 1), flagC), 255)

    if (operand ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end

    self.flags.P = self:get_parity(operand)

    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)
            
    return operand
end
Z80.do_rrc = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.band(operand, 1) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    local flagC = 0
    if self.flags.C == true then
        flagC = 1
    end
    operand = bit.bor(bit.band(bit.rshift(operand, 1), 127), bit.lshift(flagC, 7))
    if (bit.band(operand, 255) ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)

    return bit.band(operand, 255)
end
Z80.do_rl = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    local temp = 0
    if self.flags.C then temp = 1 end
    if (bit.rshift(bit.band(operand, 128), 7) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.band(bit.bor(bit.lshift(operand, 1), temp), 255)
    if (operand ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)

    return operand
end
Z80.do_rr = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    local temp = 0
    if self.flags.C then temp = 1 end
    if (bit.band(operand, 1) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.bor(bit.band(bit.rshift(operand, 1), 127), bit.lshift(temp, 7))
    if (operand ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)

    --os.exit()
    return operand
end
Z80.do_sla = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.rshift(bit.band(operand, 128), 7) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.band(bit.lshift(operand, 1), 255)
    if (not (operand > 0)) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)
    return operand
end
Z80.do_sra = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.band(operand, 1) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.bor(bit.band(bit.rshift(operand, 1), 127), bit.band(operand, 128))
    if (not (operand > 0)) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)
    return operand
end
Z80.do_sll = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.rshift(bit.band(operand, 128), 7) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.bor(bit.band(bit.lshift(operand, 1), 255), 1)
    if (not (operand > 0)) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.P = self:get_parity(operand)
    if (bit.band(operand, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    self:update_xy_flags(operand)
    return operand
end
Z80.do_srl = function(self, operand)
    self.flags.N = false
    self.flags.H = false
    if (bit.band(operand, 1) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    operand = bit.band(bit.rshift(operand, 1), 127)
    if (not (operand > 0)) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    self.flags.P = self:get_parity(operand)
    self.flags.S = 0
    self:update_xy_flags(operand)
    return operand
end
Z80.do_ix_add = function(self, operand)
    self.flags.N = false
    local result = self.ix + operand
    if (bit.band(result, 65536) ~= 0) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    if (bit.band((bit.band(self.ix, 4095) + bit.band(operand, 4095)), 4096) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    self:update_xy_flags(bit.rshift(bit.band(result, 65280), 8))
    self.ix = result
end
Z80.instructions = {}
Z80.instructions[0] = function(self)
end
Z80.instructions[1] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.c = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    self.b = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[2] = function(self)
    self.memory.mem_write(self.memory, bit.bor(self.c, bit.lshift(self.b, 8)), self.a)
end
Z80.instructions[3] = function(self)
    local result = bit.bor(self.c, bit.lshift(self.b, 8))
    result = result + 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[4] = function(self)
    self.b = self:do_inc(self.b)
end
Z80.instructions[5] = function(self)
    self.b = self:do_dec(self.b)
end
Z80.instructions[6] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.b = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[7] = function(self)
    local temp_s = self.flags.S
    temp_z = self.flags.Z
    temp_p = self.flags.P
    self.a = self:do_rlc(self.a)
    self.flags.S = temp_s
    self.flags.Z = temp_z
    self.flags.P = temp_p
end
Z80.instructions[8] = function(self)
    local temp = self.a
    self.a = self.a_prime
    self.a_prime = temp
    temp = self:get_flags_register()
    self:set_flags_register(self:get_flags_prime())
    self:set_flags_prime(temp)
end
Z80.instructions[9] = function(self)
    self:do_hl_add(bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.instructions[10] = function(self)
    self.a = self.memory.mem_read(self.memory, bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.instructions[11] = function(self)
    local result = bit.bor(self.c, bit.lshift(self.b, 8))
    result = result - 1
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[12] = function(self)
    self.c = self:do_inc(self.c)
end
Z80.instructions[13] = function(self)
    self.c = self:do_dec(self.c)
end
Z80.instructions[14] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.c = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[15] = function(self)
    local temp_s, temp_z, temp_p = self.flags.S, self.flags.Z, self.flags.P
    self.a = self:do_rrc(self.a)
    self.flags.S = temp_s
    self.flags.Z = temp_z
    self.flags.P = temp_p
end
Z80.instructions[16] = function(self)
    self.b = bit.band((self.b - 1), 255)
    self:do_conditional_relative_jump((self.b ~= 0))
end
Z80.instructions[17] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.e = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    self.d = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[18] = function(self)
    self.memory.mem_write(self.memory, bit.bor(self.e, bit.lshift(self.d, 8)), self.a)
end
Z80.instructions[19] = function(self)
    local result = bit.bor(self.e, bit.lshift(self.d, 8))
    result = result + 1
    self.e = bit.band(result, 255)
    self.d = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[20] = function(self)
    self.d = self:do_inc(self.d)
end
Z80.instructions[21] = function(self)
    self.d = self:do_dec(self.d)
end
Z80.instructions[22] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.d = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[23] = function(self)
    local temp_s, temp_z, temp_p = self.flags.S, self.flags.Z, self.flags.P
    self.a = self:do_rl(self.a)
    self.flags.S = temp_s
    self.flags.Z = temp_z
    self.flags.P = temp_p
end
Z80.instructions[24] = function(self)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)))
    self.pc = bit.band(((self.pc + offset) + 1), 65535)
end
Z80.instructions[25] = function(self)
    self:do_hl_add(bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.instructions[26] = function(self)
    self.a = self.memory.mem_read(self.memory, bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.instructions[27] = function(self)
    local result = bit.bor(self.e, bit.lshift(self.d, 8))
    result = result - 1
    self.e = bit.band(result, 255)
    self.d = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[28] = function(self)
    self.e = self:do_inc(self.e)
end
Z80.instructions[29] = function(self)
    self.e = self:do_dec(self.e)
end
Z80.instructions[30] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.e = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[31] = function(self)
    local temp_s, temp_z, temp_p = self.flags.S, self.flags.Z, self.flags.P
    self.a = self:do_rr(self.a)
    self.flags.S = temp_s
    self.flags.Z = temp_z
    self.flags.P = temp_p
end
Z80.instructions[32] = function(self)
    self:do_conditional_relative_jump(not self.flags.Z)
end
Z80.instructions[33] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.l = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    self.h = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[34] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, self.l)
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), self.h)
end
Z80.instructions[35] = function(self)
    local result = bit.bor(self.l, bit.lshift(self.h, 8))
    result = result + 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[36] = function(self)
    self.h = self:do_inc(self.h)
end
Z80.instructions[37] = function(self)
    self.h = self:do_dec(self.h)
end
Z80.instructions[38] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.h = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[39] = function(self)
    local temp = self.a
    if not self.flags.N then
        if self.flags.H or (bit.band(self.a, 15) > 9) then
            temp = temp + 6
        end
        if self.flags.C or (self.a > 153) then
            temp = temp + 96
        end
    else
        if self.flags.H or (bit.band(self.a, 15) > 9) then
            temp = temp - 6
        end
        if self.flags.C or (self.a > 153) then
            temp = temp - 96
        end
    end
    if (bit.band(temp, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if ((bit.band(temp, 255) == 0)) then
        self.flags.Z = true
    else
        self.flags.Z = false
    end
    if (bit.bxor(bit.band(self.a, 16), bit.band(temp, 16)) ~= 0) then
        self.flags.H = true
    else
        self.flags.H = false
    end
    self.flags.P = self:get_parity(bit.band(temp, 255))
    if ((self.flags.C or (self.a > 153)) == true) then
        self.flags.C = true
    else
        self.flags.C = false
    end
    self.a = bit.band(temp, 255)
    self:update_xy_flags(self.a)
end
Z80.instructions[40] = function(self)
    self:do_conditional_relative_jump(self.flags.Z)
end
Z80.instructions[41] = function(self)
    self:do_hl_add(bit.bor(self.l, bit.lshift(self.h, 8)))
end
Z80.instructions[42] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.l = self.memory.mem_read(self.memory, address)
    self.h = self.memory.mem_read(self.memory, bit.band((address + 1), 65535))
end
Z80.instructions[43] = function(self)
    local result = bit.bor(self.l, bit.lshift(self.h, 8))
    result = result - 1
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[44] = function(self)
    self.l = self:do_inc(self.l)
end
Z80.instructions[45] = function(self)
    self.l = self:do_dec(self.l)
end
Z80.instructions[46] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.l = self.memory.mem_read(self.memory, self.pc)
end
Z80.instructions[47] = function(self)
    self.a = bit.band(bit.bnot(self.a), 255)
    self.flags.N = true
    self.flags.H = true
    self:update_xy_flags(self.a)
end
Z80.instructions[48] = function(self)
    self:do_conditional_relative_jump(not self.flags.C)
end
Z80.instructions[49] = function(self)
    self.sp =
        bit.bor(
        self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)),
        bit.lshift(self.memory.mem_read(self.memory, bit.band((self.pc + 2), 65535)), 8)
    )
    self.pc = bit.band((self.pc + 2), 65535)
end
Z80.instructions[50] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, self.a)
end
Z80.instructions[51] = function(self)
    self.sp = bit.band((self.sp + 1), 65535)
end
Z80.instructions[52] = function(self)
    local address = bit.bor(self.l, bit.lshift(self.h, 8))
    self.memory.mem_write(self.memory, address, self:do_inc(self.memory.mem_read(self.memory, address)))
end
Z80.instructions[53] = function(self)
    local address = bit.bor(self.l, bit.lshift(self.h, 8))
    self.memory.mem_write(self.memory, address, self:do_dec(self.memory.mem_read(self.memory, address)))
end
Z80.instructions[54] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[55] = function(self)
    self.flags.N = false
    self.flags.H = false
    self.flags.C = true
    self:update_xy_flags(self.a)
end
Z80.instructions[56] = function(self)
    self:do_conditional_relative_jump(self.flags.C)
end
Z80.instructions[57] = function(self)
    self:do_hl_add(self.sp)
end
Z80.instructions[58] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.a = self.memory.mem_read(self.memory, address)
end
Z80.instructions[59] = function(self)
    self.sp = bit.band((self.sp - 1), 65535)
end
Z80.instructions[60] = function(self)
    self.a = self:do_inc(self.a)
end
Z80.instructions[61] = function(self)
    self.a = self:do_dec(self.a)
end
Z80.instructions[62] = function(self)
    self.a = self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535))
    self.pc = bit.band((self.pc + 1), 65535)
end
Z80.instructions[63] = function(self)
    self.flags.N = false
    self.flags.H = self.flags.C
    if (self.flags.C == true) then
        self.flags.C = false
    else
        self.flags.C = true
    end
    self:update_xy_flags(self.a)
end
Z80.instructions[192] = function(self)
    self:do_conditional_return(not self.flags.Z)
end
Z80.instructions[193] = function(self)
    local result = self:pop_word()
    self.c = bit.band(result, 255)
    self.b = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[194] = function(self)
    self:do_conditional_absolute_jump(not self.flags.Z)
end
Z80.instructions[195] = function(self)
    self.pc =
        bit.bor(
        self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)),
        bit.lshift(self.memory.mem_read(self.memory, bit.band((self.pc + 2), 65535)), 8)
    )
    self.pc = bit.band((self.pc - 1), 65535)
end
Z80.instructions[196] = function(self)
    self:do_conditional_call(not self.flags.Z)
end
Z80.instructions[197] = function(self)
    self:push_word(bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.instructions[198] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_add(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[199] = function(self)
    self:do_reset(0)
end
Z80.instructions[200] = function(self)
    self:do_conditional_return(self.flags.Z)
end
Z80.instructions[201] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
end
Z80.instructions[202] = function(self)
    self:do_conditional_absolute_jump(self.flags.Z)
end
Z80.instructions[203] = function(self)
    self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
    self.pc = bit.band((self.pc + 1), 65535)
    local opcode = self.memory.mem_read(self.memory, self.pc)
    local bit_number = bit.rshift(bit.band(opcode, 56), 3)
    local reg_code = bit.band(opcode, 7)
    if opcode < 64 then
        local op_array = {
            self.do_rlc,
            self.do_rrc,
            self.do_rl,
            self.do_rr,
            self.do_sla,
            self.do_sra,
            self.do_sll,
            self.do_srl
        }
        if reg_code == 0 then
            self.b = op_array[bit_number + 1](self, self.b)
        elseif reg_code == 1 then
            self.c = op_array[bit_number + 1](self, self.c)
        elseif reg_code == 2 then
            self.d = op_array[bit_number + 1](self, self.d)
        elseif reg_code == 3 then
            self.e = op_array[bit_number + 1](self, self.e)
        elseif reg_code == 4 then
            self.h = op_array[bit_number + 1](self, self.h)
        elseif reg_code == 5 then
            self.l = op_array[bit_number + 1](self, self.l)
        elseif reg_code == 6 then
            self.memory.mem_write(
                self,
                bit.bor(self.l, bit.lshift(self.h, 8)),
                op_array[bit_number + 1](self, self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))))
            )
        elseif reg_code == 7 then
            self.a = op_array[bit_number + 1](self, self.a)
        end
    elseif opcode < 128 then

        if reg_code == 0 then
            if (not (bit.band(self.b, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 1 then
            if (not (bit.band(self.c, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 2 then
            if (not (bit.band(self.d, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 3 then
            if (not (bit.band(self.e, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 4 then
            if (not (bit.band(self.h, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 5 then
            if (not (bit.band(self.l, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 6 then
            if (not (bit.band(self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))), bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        elseif reg_code == 7 then
            if (not (bit.band(self.a, bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
        end
        self.flags.N = false
        self.flags.H = true
        self.flags.P = self.flags.Z
        if (((bit_number == 7) and not self.flags.Z)) then
            self.flags.S = true
        else
            self.flags.S = false
        end
        if (((bit_number == 5) and not self.flags.Z)) then
            self.flags.Y = true
        else
            self.flags.Y = false
        end
        if (((bit_number == 3) and not self.flags.Z)) then
            self.flags.X = true
        else
            self.flags.X = false
        end
    elseif opcode < 192 then
        if reg_code == 0 then
            self.b = bit.band(bit.band(self.b, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 1 then
            self.c = bit.band(bit.band(self.c, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 2 then
            self.d = bit.band(bit.band(self.d, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 3 then
            self.e = bit.band(bit.band(self.e, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 4 then
            self.h = bit.band(bit.band(self.h, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 5 then
            self.l = bit.band(bit.band(self.l, 255), bit.bnot(bit.lshift(1, bit_number)))
        elseif reg_code == 6 then
            self.memory.mem_write(
                self,
                bit.bor(self.l, bit.lshift(self.h, 8)),
                bit.band(self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))), bit.bnot(bit.lshift(1, bit_number)))
            )
        elseif reg_code == 7 then
            self.a = bit.band(bit.band(self.a, 255), bit.bnot(bit.lshift(1, bit_number)))
        end
    else
        if reg_code == 0 then
            self.b = bit.bor(self.b, bit.lshift(1, bit_number))
        elseif reg_code == 1 then
            self.c = bit.bor(self.c, bit.lshift(1, bit_number))
        elseif reg_code == 2 then
            self.d = bit.bor(self.d, bit.lshift(1, bit_number))
        elseif reg_code == 3 then
            self.e = bit.bor(self.e, bit.lshift(1, bit_number))
        elseif reg_code == 4 then
            self.h = bit.bor(self.h, bit.lshift(1, bit_number))
        elseif reg_code == 5 then
            self.l = bit.bor(self.l, bit.lshift(1, bit_number))
        elseif reg_code == 6 then
            self.memory.mem_write(
                self,
                bit.bor(self.l, bit.lshift(self.h, 8)),
                bit.bor(self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8))), bit.lshift(1, bit_number))
            )
        elseif reg_code == 7 then
            self.a = bit.bor(self.a, bit.lshift(1, bit_number))
        end
    end
    self.cycle_counter = self.cycle_counter + self.cycle_counts_cb[opcode]
end
Z80.instructions[204] = function(self)
    self:do_conditional_call(self.flags.Z)
end
Z80.instructions[205] = function(self)
    self:push_word(bit.band((self.pc + 3), 65535))
    self.pc =
        bit.bor(
        self.memory.mem_read(self.memory, bit.band((self.pc + 1), 65535)),
        bit.lshift(self.memory.mem_read(self.memory, bit.band((self.pc + 2), 65535)), 8)
    )
    self.pc = bit.band((self.pc - 1), 65535)
end
Z80.instructions[206] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_adc(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[207] = function(self)
    self:do_reset(8)
end
Z80.instructions[208] = function(self)
    self:do_conditional_return(not self.flags.C)
end
Z80.instructions[209] = function(self)
    local result = self:pop_word()
    self.e = bit.band(result, 255)
    self.d = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[210] = function(self)
    self:do_conditional_absolute_jump(not self.flags.C)
end
Z80.instructions[211] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.a, 8), self.memory.mem_read(self.memory, self.pc)), self.a)
end
Z80.instructions[212] = function(self)
    self:do_conditional_call(not self.flags.C)
end
Z80.instructions[213] = function(self)
    self:push_word(bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.instructions[214] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_sub(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[215] = function(self)
    self:do_reset(16)
end
Z80.instructions[216] = function(self)
    self:do_conditional_return(self.flags.C)
end
Z80.instructions[217] = function(self)
    local temp = self.b
    self.b = self.b_prime
    self.b_prime = temp
    temp = self.c
    self.c = self.c_prime
    self.c_prime = temp
    temp = self.d
    self.d = self.d_prime
    self.d_prime = temp
    temp = self.e
    self.e = self.e_prime
    self.e_prime = temp
    temp = self.h
    self.h = self.h_prime
    self.h_prime = temp
    temp = self.l
    self.l = self.l_prime
    self.l_prime = temp
end
Z80.instructions[218] = function(self)
    self:do_conditional_absolute_jump(self.flags.C)
end
Z80.instructions[219] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.a = self.memory.io_read(self.memory, bit.bor(bit.lshift(self.a, 8), self.memory.mem_read(self.memory, self.pc)))
end
Z80.instructions[220] = function(self)
    self:do_conditional_call(self.flags.C)
end
Z80.instructions[221] = function(self)
    self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
    self.pc = bit.band((self.pc + 1), 65535)
    local opcode = self.memory.mem_read(self.memory, self.pc)
    local func = self.dd_instructions[opcode]
    if func then
        func(self)
        self.cycle_counter = self.cycle_counter + self.cycle_counts_dd[opcode]
    else
        self.pc = bit.band((self.pc - 1), 65535)
        self.cycle_counter = self.cycle_counter + self.cycle_counts[0]
    end
end
Z80.instructions[222] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_sbc(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[223] = function(self)
    self:do_reset(24)
end
Z80.instructions[224] = function(self)
    self:do_conditional_return(not self.flags.P)
end
Z80.instructions[225] = function(self)
    local result = self:pop_word()
    self.l = bit.band(result, 255)
    self.h = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[226] = function(self)
    self:do_conditional_absolute_jump(not self.flags.P)
end
Z80.instructions[227] = function(self)
    local temp = self.memory.mem_read(self.memory, self.sp)
    self.memory.mem_write(self.memory, self.sp, self.l)
    self.l = temp
    temp = self.memory.mem_read(self.memory, bit.band((self.sp + 1), 65535))
    self.memory.mem_write(self.memory, bit.band((self.sp + 1), 65535), self.h)
    self.h = temp
end
Z80.instructions[228] = function(self)
    self:do_conditional_call(not self.flags.P)
end
Z80.instructions[229] = function(self)
    self:push_word(bit.bor(self.l, bit.lshift(self.h, 8)))
end
Z80.instructions[230] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_and(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[231] = function(self)
    self:do_reset(32)
end
Z80.instructions[232] = function(self)
    self:do_conditional_return(self.flags.P)
end
Z80.instructions[233] = function(self)
    self.pc = bit.bor(self.l, bit.lshift(self.h, 8))
    self.pc = bit.band((self.pc - 1), 65535)
end
Z80.instructions[234] = function(self)
    self:do_conditional_absolute_jump(self.flags.P)
end
Z80.instructions[235] = function(self)
    local temp = self.d
    self.d = self.h
    self.h = temp
    temp = self.e
    self.e = self.l
    self.l = temp
end
Z80.instructions[236] = function(self)
    self:do_conditional_call(self.flags.P)
end
Z80.instructions[237] = function(self)
    self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
    self.pc = bit.band((self.pc + 1), 65535)
    local opcode = self.memory.mem_read(self.memory, self.pc)
    --print("OPCODE: "..opcode)
    local func = self.ed_instructions[opcode]
    if func then
        func(self)
        self.cycle_counter = self.cycle_counter + self.cycle_counts_ed[opcode]
    else
        self.cycle_counter = self.cycle_counter + self.cycle_counts[0]
    end
end
Z80.instructions[238] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_xor(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[239] = function(self)
    self:do_reset(40)
end
Z80.instructions[240] = function(self)
    self:do_conditional_return(not self.flags.S)
end
Z80.instructions[241] = function(self)
    local result = self:pop_word()
    self:set_flags_register(bit.band(result, 255))
    self.a = bit.rshift(bit.band(result, 65280), 8)
end
Z80.instructions[242] = function(self)
    self:do_conditional_absolute_jump(not self.flags.S)
end
Z80.instructions[243] = function(self)
    self.do_delayed_di = true
end
Z80.instructions[244] = function(self)
    self:do_conditional_call(not self.flags.S)
end
Z80.instructions[245] = function(self)
    self:push_word(bit.bor(self:get_flags_register(), bit.lshift(self.a, 8)))
end
Z80.instructions[246] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_or(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[247] = function(self)
    self:do_reset(48)
end
Z80.instructions[248] = function(self)
    self:do_conditional_return(self.flags.S)
end
Z80.instructions[249] = function(self)
    self.sp = bit.bor(self.l, bit.lshift(self.h, 8))
end
Z80.instructions[250] = function(self)
    self:do_conditional_absolute_jump(self.flags.S)
end
Z80.instructions[251] = function(self)
    self.do_delayed_ei = true
end
Z80.instructions[252] = function(self)
    self:do_conditional_call(self.flags.S)
end
Z80.instructions[253] = function(self)
    self.r = bit.bor(bit.band(self.r, 128), bit.band((bit.band(self.r, 127) + 1), 127))
    self.pc = bit.band((self.pc + 1), 65535)
    local opcode = self.memory.mem_read(self.memory, self.pc)
    local func = self.dd_instructions[opcode]
    if func then
        local temp = self.ix
        self.ix = iy
        func(self)
        iy = self.ix
        self.ix = temp
        self.cycle_counter = self.cycle_counter + self.cycle_counts_dd[opcode]
    else
        self.pc = bit.band((self.pc - 1), 65535)
        self.cycle_counter = self.cycle_counter + self.cycle_counts[0]
    end
end
Z80.instructions[254] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self:do_cp(self.memory.mem_read(self.memory, self.pc))
end
Z80.instructions[255] = function(self)
    self:do_reset(56)
end
Z80.ed_instructions = {}
Z80.ed_instructions[64] = function(self)
    self.b = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[65] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.b)
end
Z80.ed_instructions[66] = function(self)
    self:do_hl_sbc(bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.ed_instructions[67] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, self.c)
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), self.b)
end
Z80.ed_instructions[68] = function(self)
    self:do_neg()
end
Z80.ed_instructions[69] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[70] = function(self)
    self.imode = 0
end
Z80.ed_instructions[71] = function(self)
    i = self.a
end
Z80.ed_instructions[72] = function(self)
    self.c = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[73] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.c)
end
Z80.ed_instructions[74] = function(self)
    self:do_hl_adc(bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.ed_instructions[75] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.c = self.memory.mem_read(self.memory, address)
    self.b = self.memory.mem_read(self.memory, bit.band((address + 1), 65535))
end
Z80.ed_instructions[76] = function(self)
    self:do_neg()
end
Z80.ed_instructions[77] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
end
Z80.ed_instructions[78] = function(self)
    self.imode = 0
end
Z80.ed_instructions[79] = function(self)
    self.r = self.a
end
Z80.ed_instructions[80] = function(self)
    self.d = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[81] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.d)
end
Z80.ed_instructions[82] = function(self)
    self:do_hl_sbc(bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.ed_instructions[83] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, self.e)
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), self.d)
end
Z80.ed_instructions[84] = function(self)
    self:do_neg()
end
Z80.ed_instructions[85] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[86] = function(self)
    self.imode = 1
end
Z80.ed_instructions[87] = function(self)
    self.a = i
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a == 1) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.H = false
    self.flags.P = self.iff2
    self.flags.N = false
    self:update_xy_flags(self.a)
end
Z80.ed_instructions[88] = function(self)
    self.e = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[89] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.e)
end
Z80.ed_instructions[90] = function(self)
    self:do_hl_adc(bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.ed_instructions[91] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.e = self.memory.mem_read(self.memory, address)
    self.d = self.memory.mem_read(self.memory, bit.band((address + 1), 65535))
end
Z80.ed_instructions[92] = function(self)
    self:do_neg()
end
Z80.ed_instructions[93] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[94] = function(self)
    self.imode = 2
end
Z80.ed_instructions[95] = function(self)
    self.a = self.r
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a == 1) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.H = false
    self.flags.P = self.iff2
    self.flags.N = false
    self:update_xy_flags(self.a)
end
Z80.ed_instructions[96] = function(self)
    self.h = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[97] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.h)
end
Z80.ed_instructions[98] = function(self)
    self:do_hl_sbc(bit.bor(self.l, bit.lshift(self.h, 8)))
end
Z80.ed_instructions[99] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, self.l)
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), self.h)
end
Z80.ed_instructions[100] = function(self)
    self:do_neg()
end
Z80.ed_instructions[101] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[102] = function(self)
    self.imode = 0
end
Z80.ed_instructions[103] = function(self)
    local hl_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    local temp1, temp2 = bit.band(hl_value, 15), bit.band(self.a, 15)
    hl_value = bit.bor(bit.rshift(bit.band(hl_value, 240), 4), bit.lshift(temp2, 4))
    self.a = bit.bor(bit.band(self.a, 240), temp1)
    self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), hl_value)
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a > 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.H = false
    if (self:get_parity(self.a) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    self:update_xy_flags(self.a)
end
Z80.ed_instructions[104] = function(self)
    self.l = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[105] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.l)
end
Z80.ed_instructions[106] = function(self)
    self:do_hl_adc(bit.bor(self.l, bit.lshift(self.h, 8)))
end
Z80.ed_instructions[107] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.l = self.memory.mem_read(self.memory, address)
    self.h = self.memory.mem_read(self.memory, bit.band((address + 1), 65535))
end
Z80.ed_instructions[108] = function(self)
    self:do_neg()
end
Z80.ed_instructions[109] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[110] = function(self)
    self.imode = 0
end
Z80.ed_instructions[111] = function(self)
    local hl_value = self.memory.mem_read(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)))
    local temp1, temp2 = bit.band(hl_value, 240), bit.band(self.a, 15)
    hl_value = bit.bor(bit.lshift(bit.band(hl_value, 15), 4), temp2)
    self.a = bit.bor(bit.band(self.a, 240), bit.rshift(temp1, 4))
    self.memory.mem_write(self.memory, bit.bor(self.l, bit.lshift(self.h, 8)), hl_value)
    if (bit.band(self.a, 128) > 0) then
        self.flags.S = true
    else
        self.flags.S = false
    end
    if (self.a ~= 0) then
        self.flags.Z = false
    else
        self.flags.Z = true
    end
    self.flags.H = false
    if (self:get_parity(self.a) == true) then
        self.flags.P = true
    else
        self.flags.P = false
    end
    self.flags.N = false
    self:update_xy_flags(self.a)
end
Z80.ed_instructions[112] = function(self)
    self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[113] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), 0)
end
Z80.ed_instructions[114] = function(self)
    self:do_hl_sbc(self.sp)
end
Z80.ed_instructions[115] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, bit.band(self.sp, 255))
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), bit.band(bit.rshift(self.sp, 8), 255))
end
Z80.ed_instructions[116] = function(self)
    self:do_neg()
end
Z80.ed_instructions[117] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[118] = function(self)
    self.imode = 1
end
Z80.ed_instructions[120] = function(self)
    self.a = self:do_in(bit.bor(bit.lshift(self.b, 8), self.c))
end
Z80.ed_instructions[121] = function(self)
    self.memory.io_write(self.memory, bit.bor(bit.lshift(self.b, 8), self.c), self.a)
end
Z80.ed_instructions[122] = function(self)
    self:do_hl_adc(self.sp)
end
Z80.ed_instructions[123] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.sp = self.memory.mem_read(self.memory, address)
    self.sp = bit.bor(self.sp, bit.lshift(self.memory.mem_read(self.memory, bit.band((address + 1), 65535)), 8))
end
Z80.ed_instructions[124] = function(self)
    self:do_neg()
end
Z80.ed_instructions[125] = function(self)
    self.pc = bit.band((self:pop_word() - 1), 65535)
    self.iff1 = self.iff2
end
Z80.ed_instructions[126] = function(self)
    self.imode = 2
end
Z80.ed_instructions[160] = function(self)
    self:do_ldi()
end
Z80.ed_instructions[161] = function(self)
    self:do_cpi()
end
Z80.ed_instructions[162] = function(self)
    self:do_ini()
end
Z80.ed_instructions[163] = function(self)
    self:do_outi()
end
Z80.ed_instructions[168] = function(self)
    self:do_ldd()
end
Z80.ed_instructions[169] = function(self)
    self:do_cpd()
end
Z80.ed_instructions[170] = function(self)
    self:do_ind()
end
Z80.ed_instructions[171] = function(self)
    self:do_outd()
end
Z80.ed_instructions[176] = function(self)
    self:do_ldi()
    if self.b ~= 0 or self.c ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[177] = function(self)
    self:do_cpi()
    if not self.flags.Z and (self.b ~= 0 or self.c ~= 0) then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[178] = function(self)
    self:do_ini()
    if self.b ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[179] = function(self)
    self:do_outi()
    if self.b ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[184] = function(self)
    self:do_ldd()
    if self.b ~= 0 or self.c ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[185] = function(self)
    self:do_cpd()
    if not self.flags.Z and (self.b ~= 0 or self.c ~= 0) then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[186] = function(self)
    self:do_ind()
    if self.b ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.ed_instructions[187] = function(self)
    self:do_outd()
    if self.b ~= 0 then
        self.cycle_counter = self.cycle_counter + 5
        self.pc = bit.band((self.pc - 2), 65535)
    end
end
Z80.dd_instructions = {}
Z80.dd_instructions[9] = function(self)
    self:do_ix_add(bit.bor(self.c, bit.lshift(self.b, 8)))
end
Z80.dd_instructions[25] = function(self)
    self:do_ix_add(bit.bor(self.e, bit.lshift(self.d, 8)))
end
Z80.dd_instructions[33] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.ix = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    self.ix = bit.bor(self.ix, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
end
Z80.dd_instructions[34] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.memory.mem_write(self.memory, address, bit.band(self.ix, 255))
    self.memory.mem_write(self.memory, bit.band((address + 1), 65535), bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[35] = function(self)
    self.ix = bit.band((self.ix + 1), 65535)
end
Z80.dd_instructions[36] = function(self)
    self.ix = bit.bor(bit.lshift(self:do_inc(bit.rshift(self.ix, 8)), 8), bit.band(self.ix, 255))
end
Z80.dd_instructions[37] = function(self)
    self.ix = bit.bor(bit.lshift(self:do_dec(bit.rshift(self.ix, 8)), 8), bit.band(self.ix, 255))
end
Z80.dd_instructions[38] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.ix = bit.bor(bit.lshift(self.memory.mem_read(self.memory, self.pc), 8), bit.band(self.ix, 255))
end
Z80.dd_instructions[41] = function(self)
    self:do_ix_add(self.ix)
end
Z80.dd_instructions[42] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local address = self.memory.mem_read(self.memory, self.pc)
    self.pc = bit.band((self.pc + 1), 65535)
    address = bit.bor(address, bit.lshift(self.memory.mem_read(self.memory, self.pc), 8))
    self.ix = self.memory.mem_read(self.memory, address)
    self.ix = bit.bor(self.ix, bit.lshift(self.memory.mem_read(self.memory, bit.band((address + 1), 65535)), 8))
end
Z80.dd_instructions[43] = function(self)
    self.ix = bit.band((self.ix - 1), 65535)
end
Z80.dd_instructions[44] = function(self)
    self.ix = bit.bor(self:do_inc(bit.band(self.ix, 255)), bit.band(self.ix, 65280))
end
Z80.dd_instructions[45] = function(self)
    self.ix = bit.bor(self:do_dec(bit.band(self.ix, 255)), bit.band(self.ix, 65280))
end
Z80.dd_instructions[46] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    self.ix = bit.bor(bit.band(self.memory.mem_read(self.memory, self.pc), 255), bit.band(self.ix, 65280))
end
Z80.dd_instructions[52] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    local value = self.memory.mem_read(self.memory, bit.band((offset + self.ix), 65535))
    self.memory.mem_write(self.memory, bit.band((offset + self.ix), 65535), self:do_inc(value))
end
Z80.dd_instructions[53] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    local value = self.memory.mem_read(self.memory, bit.band((offset + self.ix), 65535))
    self.memory.mem_write(self.memory, bit.band((offset + self.ix), 65535), self:do_dec(value))
end
Z80.dd_instructions[54] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.pc = bit.band((self.pc + 1), 65535)
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.memory.mem_read(self.memory, self.pc))
end
Z80.dd_instructions[57] = function(self)
    self:do_ix_add(self.sp)
end
Z80.dd_instructions[68] = function(self)
    self.b = bit.band(bit.rshift(self.ix, 8), 255)
end
Z80.dd_instructions[69] = function(self)
    self.b = bit.band(self.ix, 255)
end
Z80.dd_instructions[70] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.b = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[76] = function(self)
    self.c = bit.band(bit.rshift(self.ix, 8), 255)
end
Z80.dd_instructions[77] = function(self)
    self.c = bit.band(self.ix, 255)
end
Z80.dd_instructions[78] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.c = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[84] = function(self)
    self.d = bit.band(bit.rshift(self.ix, 8), 255)
end
Z80.dd_instructions[85] = function(self)
    self.d = bit.band(self.ix, 255)
end
Z80.dd_instructions[86] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.d = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[92] = function(self)
    self.e = bit.band(bit.rshift(self.ix, 8), 255)
end
Z80.dd_instructions[93] = function(self)
    self.e = bit.band(self.ix, 255)
end
Z80.dd_instructions[94] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.e = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[96] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(self.b, 8))
end
Z80.dd_instructions[97] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(self.c, 8))
end
Z80.dd_instructions[98] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(self.d, 8))
end
Z80.dd_instructions[99] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(self.e, 8))
end
Z80.dd_instructions[100] = function(self)
end
Z80.dd_instructions[101] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(bit.band(self.ix, 255), 8))
end
Z80.dd_instructions[102] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.h = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[103] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 255), bit.lshift(self.a, 8))
end
Z80.dd_instructions[104] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), self.b)
end
Z80.dd_instructions[105] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), self.c)
end
Z80.dd_instructions[106] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), self.d)
end
Z80.dd_instructions[107] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), self.e)
end
Z80.dd_instructions[108] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), bit.rshift(self.ix, 8))
end
Z80.dd_instructions[109] = function(self)
end
Z80.dd_instructions[110] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.l = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[111] = function(self)
    self.ix = bit.bor(bit.band(self.ix, 65280), self.a)
end
Z80.dd_instructions[112] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.b)
end
Z80.dd_instructions[113] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.c)
end
Z80.dd_instructions[114] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.d)
end
Z80.dd_instructions[115] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.e)
end
Z80.dd_instructions[116] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.h)
end
Z80.dd_instructions[117] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.l)
end
Z80.dd_instructions[119] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), self.a)
end
Z80.dd_instructions[124] = function(self)
    self.a = bit.band(bit.rshift(self.ix, 8), 255)
end
Z80.dd_instructions[125] = function(self)
    self.a = bit.band(self.ix, 255)
end
Z80.dd_instructions[126] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.a = self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535))
end
Z80.dd_instructions[132] = function(self)
    self:do_add(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[133] = function(self)
    self:do_add(bit.band(self.ix, 255))
end
Z80.dd_instructions[134] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_add(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[140] = function(self)
    self:do_adc(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[141] = function(self)
    self:do_adc(bit.band(self.ix, 255))
end
Z80.dd_instructions[142] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_adc(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[148] = function(self)
    self:do_sub(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[149] = function(self)
    self:do_sub(bit.band(self.ix, 255))
end
Z80.dd_instructions[150] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_sub(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[156] = function(self)
    self:do_sbc(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[157] = function(self)
    self:do_sbc(bit.band(self.ix, 255))
end
Z80.dd_instructions[158] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_sbc(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[164] = function(self)
    self:do_and(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[165] = function(self)
    self:do_and(bit.band(self.ix, 255))
end
Z80.dd_instructions[166] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_and(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[172] = function(self)
    self:do_xor(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[173] = function(self)
    self:do_xor(bit.band(self.ix, 255))
end
Z80.dd_instructions[174] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_xor(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[180] = function(self)
    self:do_or(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[181] = function(self)
    self:do_or(bit.band(self.ix, 255))
end
Z80.dd_instructions[182] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)

    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))

    self:do_or(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[188] = function(self)
    self:do_cp(bit.band(bit.rshift(self.ix, 8), 255))
end
Z80.dd_instructions[189] = function(self)
    self:do_cp(bit.band(self.ix, 255))
end
Z80.dd_instructions[190] = function(self)
    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self:do_cp(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
end
Z80.dd_instructions[203] = function(self)

    self.pc = bit.band((self.pc + 1), 65535)
    local offset = self:get_signed_offset_byte(self.memory.mem_read(self.memory, self.pc))
    self.pc = bit.band((self.pc + 1), 65535)
    local opcode, value = self.memory.mem_read(self.memory, self.pc), nil
    if opcode < 64 then
        local ddcb_functions = {
            self.do_rlc,
            self.do_rrc,
            self.do_rl,
            self.do_rr,
            self.do_sla,
            self.do_sra,
            self.do_sll,
            self.do_srl
        }
        local func = ddcb_functions[bit.rshift(bit.band(opcode, 56), 3) + 1]
        value = func(self, self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)))
        self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), value)
    else
        local bit_number = bit.rshift(bit.band(opcode, 56), 3)
        if opcode < 128 then
            self.flags.N = false
            self.flags.H = true
            if (not (bit.band(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)), bit.lshift(1, bit_number)) ~= 0)) then
                self.flags.Z = true
            else
                self.flags.Z = false
            end
            self.flags.P = self.flags.Z
            if (((bit_number == 7) and not self.flags.Z)) then
                self.flags.S = true
            else
                self.flags.S = false
            end
        elseif opcode < 192 then
            value =
                bit.band(
                bit.band(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)), bit.bnot(bit.lshift(1, bit_number))),
                255
            )
            self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), value)
        else
            value = bit.bor(self.memory.mem_read(self.memory, bit.band((self.ix + offset), 65535)), bit.lshift(1, bit_number))
            self.memory.mem_write(self.memory, bit.band((self.ix + offset), 65535), value)
        end
    end
    if value ~= nil then
        if bit.band(opcode, 7) == 0 then
            self.b = value
        elseif bit.band(opcode, 7) == 1 then
            self.c = value
        elseif bit.band(opcode, 7) == 2 then
            self.d = value
        elseif bit.band(opcode, 7) == 3 then
            self.e = value
        elseif bit.band(opcode, 7) == 4 then
            self.h = value
        elseif bit.band(opcode, 7) == 5 then
            self.l = value
        elseif bit.band(opcode, 7) == 7 then
            self.a = value
        end
    end
    self.cycle_counter = self.cycle_counter + self.cycle_counts_cb[opcode] + 8
end
Z80.dd_instructions[225] = function(self)
    self.ix = self:pop_word()
end
Z80.dd_instructions[227] = function(self)
    local temp = self.ix
    self.ix = self.memory.mem_read(self.memory, self.sp)
    self.ix = bit.bor(self.ix, bit.lshift(self.memory.mem_read(self.memory, bit.band((self.sp + 1), 65535)), 8))
    self.memory.mem_write(self.memory, self.sp, bit.band(temp, 255))
    self.memory.mem_write(self.memory, bit.band((self.sp + 1), 65535), bit.band(bit.rshift(temp, 8), 255))
end
Z80.dd_instructions[229] = function(self)
    self:push_word(self.ix)
end
Z80.dd_instructions[233] = function(self)
    self.pc = bit.band((self.ix - 1), 65535)
end
Z80.dd_instructions[249] = function(self)
    self.sp = self.ix
end
Z80.cycle_counts_oneBased = {
    4, 10, 7, 6, 4, 4, 7, 4, 4, 11, 7, 6, 4, 4, 7, 4, 8, 10, 7, 6, 4, 4, 7, 4, 12, 11, 7, 6, 4, 4, 7, 4, 7, 10, 16, 6, 4, 4, 7, 4, 7, 11, 16, 6, 4, 4, 7, 4, 7, 10, 13, 6, 11, 11, 10, 4, 7, 11, 13, 6, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 7, 7, 7, 7, 7, 7, 4, 7, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 5, 10, 10, 10, 10, 11, 7, 11, 5, 10, 10, 0, 10, 17, 7, 11, 5, 10, 10, 11, 10, 11, 7, 11, 5, 4, 10, 11, 10, 0, 7, 11, 5, 10, 10, 19, 10, 11, 7, 11, 5, 4, 10, 4, 10, 0, 7, 11, 5, 10, 10, 4, 10, 11, 7, 11, 5, 6, 10, 4, 10, 0, 7, 11
}
Z80.cycle_counts_ed_oneBased = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 12, 15, 20, 8, 14, 8, 9, 12, 12, 15, 20, 8, 14, 8, 9, 12, 12, 15, 20, 8, 14, 8, 9, 12, 12, 15, 20, 8, 14, 8, 9, 12, 12, 15, 20, 8, 14, 8, 18, 12, 12, 15, 20, 8, 14, 8, 18, 12, 12, 15, 20, 8, 14, 8, 0, 12, 12, 15, 20, 8, 14, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 16, 0, 0, 0, 0, 16, 16, 16, 16, 0, 0, 0, 0, 16, 16, 16, 16, 0, 0, 0, 0, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
}
Z80.cycle_counts_cb_oneBased = {
    8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8, 8, 8, 8, 8, 8, 8, 15, 8
}
Z80.cycle_counts_dd_oneBased = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 14, 20, 10, 8, 8, 11, 0, 0, 15, 20, 10, 8, 8, 11, 0, 0, 0, 0, 0, 23, 23, 19, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8, 8, 8, 8, 8, 19, 8, 19, 19, 19, 19, 19, 19, 0, 19, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 8, 8, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0, 23, 0, 15, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0
}
Z80.cycle_counts = {}
Z80.cycle_counts_ed = {}
Z80.cycle_counts_cb = {}
Z80.cycle_counts_dd = {}

for i = 0, #Z80.cycle_counts_oneBased - 1 do
    Z80.cycle_counts[i] = Z80.cycle_counts_oneBased[i + 1]
end
for i = 0, #Z80.cycle_counts_ed_oneBased - 1 do
    Z80.cycle_counts_ed[i] = Z80.cycle_counts_ed_oneBased[i + 1]
end
for i = 0, #Z80.cycle_counts_cb_oneBased - 1 do
    Z80.cycle_counts_cb[i] = Z80.cycle_counts_cb_oneBased[i + 1]
end
for i = 0, #Z80.cycle_counts_dd_oneBased - 1 do
    Z80.cycle_counts_dd[i] = Z80.cycle_counts_dd_oneBased[i + 1]
end

