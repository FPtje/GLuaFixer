.cabal-sandbox\bin\uuagc.exe --haskellsyntax --data src/GLua/AG/AST.ag
.cabal-sandbox\bin\uuagc.exe --haskellsyntax --data --strictdata src/GLua/AG/Token.ag
.cabal-sandbox\bin\uuagc.exe --catas --haskellsyntax --semfuns --wrappers --signatures src/GLua/AG/PrettyPrint.ag
.cabal-sandbox\bin\uuagc.exe --catas --haskellsyntax --semfuns --wrappers --signatures src/GLuaFixer/AG/DarkRPRewrite.ag
.cabal-sandbox\bin\uuagc.exe --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/LexLint.ag
.cabal-sandbox\bin\uuagc.exe --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/ASTLint.ag
