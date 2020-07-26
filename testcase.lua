-- Lua test file for basic regression testing.
-- Run glualint --test testcase.lua to run the test

a = {}
a = {1}
a = {1,2,3}
a = {
  foo, bar;
  baz
}
a = {
  foo, bar;
  baz,
}
a = {
  foo, bar;
  baz;
}
a = {
  [1] = 1,
  [2] = 3;
}
a = {
  foo = 1,
  2,
  bar = 3
}
a = {
  {1,2,3},
  {1,2,3}
}
a = {
  [1] = 1,
  2, 3,
  [4] = 4
}
a = {
  a, b;
  c
}
a = {
  --[[foo]] a, b;
  c
}
a = {
  -- foo
  a, b;
  c
}
a = {
  a-- foo
  , b;
  c
}

local t = {
  val1, val2;
  val3, val4;
}

local t = {
  [0] = val1, val2;
  val3, val4;
}

-- Ambiguous without ; as bruh(print)(a) is a valid call
bruh = function() end
a, b = bruh;
(print)(a)

-- NOT Ambiguous without ; as true(print)(a) is syntactically invalid
a, b = true;
(print)(a)

-- Ambiguous without ; as bruh(print)(a) is a valid call
a, b = true, bruh;
(print)(a)

-- Obviously not ambiguous without ;
a, b = bruh;
print(a)

--  Ambiguous without ; as bruh()(print)(a) is a valid call
bruh();
(print)(a)

a, b = bruh, true;
(print)(a)

-- https://github.com/FPtje/GLuaFixer/issues/55
if false then
  -- print("TODO")
end

microwave = "전자렌지"

-- https://github.com/FPtje/GLuaFixer/commit/445638db4c9f9a198f18846bd8bfb1e58b0f3a99
a = { function() return end }

-- https://github.com/FPtje/GLuaFixer/issues/45
if (amount and isnumber(amount) and amount > 0 and char) then
    if (char:hasReserve(amount)) then
        -- Fee 10%
    end
else
    client = {notify = function() end}
    L = function() end
    client:notify(L("provideValidNumber", client))
end

-- https://github.com/FPtje/GLuaFixer/issues/41
if true then
return false end -- a

-- https://github.com/FPtje/GLuaFixer/issues/25
tbl={1,2}
foo = 5^#tbl

-- https://github.com/FPtje/GLuaFixer/issues/2
a = -.5
f = 0.5

:: foo ::
::

bar

::

::baz::

foo = 0000010
hex = 0X01234567890abcdef
hex_decimal = 0X01234567890abcdef.5

-- Binary literals, added in LuaJIT 2.1.0
-- https://github.com/FPtje/GLuaFixer/issues/88
a = 0b100101010
b = 0b0
c = 0b01010101
d = 0b11111
d = 0b00000

-- LL and ULL for signed and unsigned 64 bit integers respectively
a = 10LL
b = 10ULL
c = 0xaefAEFlL
d = 0XaefAEFull
e = 0b0110ll
f = 0b0110uLl

-- Imaginary numbers
a = 10i
b = 10I
c = 0xfi
d = 0xfI
e = 0b01i

