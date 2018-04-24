Read Fallout save file.
=======================

Currently handling Fallout 1 only.

Based on this research: falloutmods.wikia.com/wiki/SAVE.DAT_File_Format

Example usage:

    % ./ReadSave.hs SLOT06
    Header {version = (1,2), characterName = "Bulgur", saveName = "Zero save.",
    saveTime = "\NUL\ETB\NUL\EOT\a\226\NUL\NUL\NUL\RS", gameTime = 264606, mapName
    = "V13ENT.sav"}

