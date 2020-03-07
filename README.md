glualint
==========

glualint - Linter and pretty printer for Garry's Mod's variant of Lua.

# Installing
1. Download the latest version of `glualint` from [the releases page](https://github.com/FPtje/GLuaFixer/releases) OR [compile it yourself](https://github.com/FPtje/GLuaFixer/blob/master/Compiling.md).
2. Place the `glualint` executable inside some folder.
3. Add the folder you put `glualint` in to your `PATH`. How this is done differs per operating system. If you're not sure how to do this, please Google `"Add to path <YOUR OS>"`.
4. Make sure you restart any terminals or text editors you currently have open.

After performing these steps, you can run `glualint` from the terminal or let your text editor use it as your linter. **Failing to specifically perform the third step will make `glualint` very unlikely to work**.

# Command line parameters

Parameter | Description
----------|------------
`--version` | Returns the version of glualint.
`--config` | Set to a `glualint.json` file for glualint configuration. See "Configuring glualint".
`--pretty-print` | Will pretty-print (re-structure/re-indent) all code given in stdin. When entering code in terminal, press Ctrl+D to finish the input.
`--pretty-print-files` | As above but pretty-prints the specified file or directory instead of stdin.
`--indentation='something'` | For pretty-print: indents all pretty-printed code with the given string. Four spaces by default, should probably some amount of tabs or spaces.
`--stdin` | Process stdin instead of specified file or directory when linting.
`--analyse-globals` | Show global variables/functions that this file defines.

# Configuring glualint
`glualint` Allows some configuration. This is done through a file called `glualint.json` or `.glualint.json`. `glualint` looks for this file in three places (in order of priority)

- The file you give to the `--config` parameter (when using the terminal)
- Any folder above the file you're working in (e.g. the root of your project)
- Your home folder, which is `C:\users\yourusername\.glualint.json` on Windows or `/home/yourusername/.glualint.json` on Unix.

Note: The file **must** either be called `glualint.json` or `.glualint.json`.

Example `glualint.json` with the default options:
```json
{
    "lint_maxScopeDepth":               7,
    "lint_syntaxErrors":                true,
    "lint_syntaxInconsistencies":       true,
    "lint_deprecated":                  true,
    "lint_trailingWhitespace":          true,
    "lint_whitespaceStyle":             true,
    "lint_beginnerMistakes":            true,
    "lint_emptyBlocks":                 true,
    "lint_shadowing":                   true,
    "lint_gotos":                       true,
    "lint_doubleNegations":             true,
    "lint_redundantIfStatements":       true,
    "lint_redundantParentheses":        true,
    "lint_duplicateTableKeys":          true,
    "lint_profanity":                   true,
    "lint_unusedVars":                  true,
    "lint_unusedParameters":            false,
    "lint_unusedLoopVars":              false,
    "lint_ignoreFiles":                 [],

    "prettyprint_spaceAfterParens":     false,
    "prettyprint_spaceAfterBrackets":   false,
    "prettyprint_spaceAfterBraces":     false,
    "prettyprint_spaceBeforeComma":     false,
    "prettyprint_spaceAfterComma":      true,
    "prettyprint_semicolons":           false,
    "prettyprint_cStyle":               false,
    "prettyprint_rejectInvalidCode":    false,
    "prettyprint_indentation":          "    "
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
- `lint_profanity`: Warn for profanity (bitch, cock, cocks, cunt, dick, dicks, fuck, fucking, goddamnit, knob, knobs, motherfucker, nigger, niggers, niggertits, nipple, shit)
- `lint_unusedVars`: Warn for variables that are never used
- `lint_unusedParameters`: Warn for function parameters that are never used. *NOTE:* Only has effect when `lint_unusedVars` is enabled!
- `lint_unusedLoopVars`: Warn for loop variables that are never used (`for k,v in ...`). *NOTE:* Only has effect when `lint_unusedVars` is enabled!
- `lint_ignoreFiles`: Files to ignore when linting. You can use glob patterns (e.g. `"*.lua"` or `libraries/*.lua`)

## Pretty print options
These options affect the pretty printing functionality of `glualint`.

- `prettyprint_spaceAfterParens`: Put a space between all parentheses
- `prettyprint_spaceAfterBrackets`: Put a space between all brackets
- `prettyprint_spaceAfterBraces`: Put a space between all curly braces
- `prettyprint_semicolons`: Clutter the script with semicolons after every damn statement
- `prettyprint_cStyle`: Use C style operators and comments everywhere
- `prettyprint_indentation`: What to use for indentation. Any string is valid, but some amount of spaces or `"\t"` is recommended
- `prettyprint_spaceBeforeComma`: Whether to place a space before every comma
- `prettyprint_spaceAfterComma`: Whether to place a space after every comma
