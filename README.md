glualint
==========

glualint - Linter and pretty printer for Garry's Mod's variant of Lua. 

# Installing
1. Download the latest version of `glualint` from [the releases page](https://github.com/FPtje/GLuaFixer/releases).
2. Place the `glualint` executable inside some folder
3. Add the folder you put `glualint` in to your `PATH`. How this is done differs per operating system. When not sure how to do this, please Google `"Add to path <YOUR OS>"`

After performing these steps, you can run `glualint` from the terminal or let your text editor use it as your linter. **Failing to specifically perform the third step will make `glualint` very unlikely to work**.

# Configuring glualint
`glualint` Allows some configuration. This is done through a file called `glualint.json`. `glualint` looks for this file in three places (in order of priority)

- The file you give to the `--config` parameter (e.g. `glualint --config "C:\glualint.json"` will load `"C:\glualint.json"`)
- Any folder above the file you're working in (e.g. when you're working in `garrysmod\gamemodes\darkrp\libraries\fn.lua` it will automatically find `garrysmod\gamemode\darkrp\glualint.json` if it's there)
- Your home folder, which is `C:\users\yourusername\glualint.json` on Windows or `/users/yourusername/glualint.json` on Unix.

Note: Calling the file anything other than `glualint.json` will make `glualint` not read it.

Example `glualint.json` with the default options:
```json
{
	"lint_maxScopeDepth":				7,
	"lint_syntaxErrors":				true,
	"lint_syntaxInconsistencies":		true,
	"lint_deprecated":					true,
	"lint_whitespaceStyle":				true,
	"lint_beginnerMistakes":			true,
	"lint_emptyBlocks":					true,
	"lint_shadowing":					true,
	"lint_gotos":						true,
	"lint_doubleNegations":				true,
	"lint_duplicateTableKeys":			true,

	"prettyprint_spaceAfterParens": 	false,
	"prettyprint_spaceAfterBrackets": 	false,
	"prettyprint_spaceAfterBraces": 	false,
	"prettyprint_semicolons": 			false,
	"prettyprint_cStyle": 				false
}
```

# All options explained:

## Linter options
- `lint_maxScopeDepth`: Maximum depth of scopes in your code. Any terrible scripter can build the most atrocious sideways code pyramids, usually without knowing. The number here is at which step the linter will start calling you out on it. Set to `0` if you're king Tut and want to disable it.
- `lint_syntaxErrors`: Whether syntax errors should be reported. This is off by default because [`gluac`](https://github.com/cartman300/gluac) shows nicer syntax errors.
- `lint_syntaxInconsistencies`: Warn for syntax inconsistencies (using both `&&` and `and`, that kind of stuff)
- `lint_deprecated`: Warn for deprecated functions. These functions are taken from [the GMod wiki](http://wiki.garrysmod.com/page/Category:Deprecated_Functions), don't blame me for the things that are on there.
- `lint_whitespaceStyle`: Warn for shitty whitespace behaviour (e.g. lack of spaces around operators and keywords)
- `lint_beginnerMistakes`: Warn for typical beginner mistakes (using self in non-metafunction, `net.WriteEntity(LocalPlayer())` in a net message, using self.Weapon in a SWEP etc.)
- `lint_emptyBlocks`: Warn for empty blocks
- `lint_shadowing`: Warn for variable shadowing
- `lint_gotos`: Warn for inappropriate gotos (i.e. the ones not used to jump out of a double loop)
- `lint_doubleNegations`: Warn for double negations (things like `not (a == b)`)
- `lint_duplicateTableKeys`: Warn for duplicate table keys (e.g. `{a = 1, a = 2}`)

# Pretty print options
These options affect the pretty printing functionality of `glualint`. 

- `prettyprint_spaceAfterParens`: Put a space between all parentheses
- `prettyprint_spaceAfterBrackets`: Put a space between all brackets
- `prettyprint_spaceAfterBraces`: Put a space between all curly braces
- `prettyprint_semicolons`: Clutter the script with semicolons after every damn statement
- `prettyprint_cStyle`: Use C style operators and comments everywhere
