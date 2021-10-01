1. compiler modflow: aller dans le repertoire et executer ./compile
2. deplacer l'executable modflw96 a la racine et dans modflw96_MAC ou modflw96_LINUX
3. editer feval.f pour mettre ./modflw96_MAC ou _LINUX
4. compiler avec ./compile
5. renommer truth.exe avec _MAC ou _LINUX --> executable truth_XXX.exe pour une run locale

Pour le runner:

6. editer bb_runner.cpp, compiler et renommer avec _MAC ou _LINUX --> executable bb_runner_XXX.exe
