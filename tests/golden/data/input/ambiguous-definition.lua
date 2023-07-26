-- Ambiguous without ; as foo(print)(a) is a valid call
foo = function() end
a, b = foo;
(print)(a)

-- NOT Ambiguous without ; as true(print)(a) is syntactically invalid
a, b = true;
(print)(a)

-- Ambiguous without ; as foo(print)(a) is a valid call
a, b = true, foo;
(print)(a)

-- Obviously not ambiguous without ;
a, b = foo;
print(a)

--  Ambiguous without ; as foo()(print)(a) is a valid call
foo();
(print)(a)

a, b = foo, true;
(print)(a)
