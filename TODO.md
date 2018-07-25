## bugs

* why scrollbar not shown??


## in roadmap

clean up rich
make rich returns a iteratable atomic

## must implementation changes

* change to a better networking for change, server pushed content, or websocket
* change command panel to a [n] k char etc.

## improvements

* clean up interaction of info and command too nasty
* currently a lot of methods is linear, scan for grapheme boundary, or scan for atomic words, also get info at index
    * `Rich.scala`
    * `Unicode.Scala`
* deal away with the hacky unicode transformation of `operation.Rich implementations`
    * abstract `cursor.Rich`
    * abstract `range.Rich`
    * make operations works with these
    * rigid `Rich` type
        * no empty plain
        * no two plain nearby in a `Text` list

## done

* ~~rebase~~
* ~~fix rebase -- again! it is tricky~~
* ~~chain rebase~~
* ~~rebase with partial success~~
* ~~server communication~~
* ~~basic viewing ui~~
* ~~basic generated ot - replace old Node and Change class~~
* ~~wire up with old code, non-generalized static server part~~
* ~~FIX THE CLIENT TEST~~
* ~~**reboot**~~
* ~~rich view present~~
* ~~mode definition is kinda of wrong? -- not wrong, we are sticking with the linearized formation for now~~
* ~~solve the unicode glyph problem~~
* ~~experiment with cursor rendering~~
* ~~motion commands~~
* ~~vim base structure~~
* ~~insert mode~~
* ~~node level sync and operations~~
* ~~fix empty document~~
* ~~visual mode~~
* ~~node visual~~
* ~~visual node delete~~
* ~~key sequence: dd~~
* ~~repeatable: test if find works ok~~
* ~~visual delete~~
* ~~wrap simple range~~
* ~~wrap link~~
* ~~smart styling~~
* ~~temp save content~~
* ~~visit links~~
* ~~node move~~
* ~~available commands panel~~
* ~~fix focus~~
* ~~single edit commands~~
* ~~keyboard editor for rich~~
* ~~fix layout - left resizeable panel, right top: DocumentView, right bottom: BottomBarView~~
* ~~rebase move~~
* ~~online user counter~~







difference with VIM

commands assuming a monospace font is generally not implemented, for example go to column N

commands related to ignoring spaces etc. is merged with the version not ignoring spaces. this is because the structure of the document now is not expressed by spaces, but by node parent-child structure

commands working with paragraphs, sections is not implemented, reason the same with above

commands working with source code (back to beginning of method etc.) is not implemented in rich text editor, the code editor is a embedded CodeMirror instance. code notes handles it's own VIM commands

we don't take special care for empty lines, for example a empty line is not considered a word when moving by word

commands which can edit multiple lines the same time (e.g. `5i`, `5o`, `5c`) cannot do this here, these are commonly used when editing code, not rich text

commands dealing with lines and blocks is merged (`V` and `Ctrl+V`)

`$` is not repeatable

`TODO` as you implement more commands, consult Vimflowy inconsistency page



