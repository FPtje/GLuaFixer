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

:: foo ::
::

bar

::

::baz::
