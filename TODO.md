
## roadmap

changes
backspace by words etc.

## bugs

* why scrollbar not shown??
* resizing the panel is buggy


## improvements

* change to a better networking for change, server pushed content, or websocket
* change command panel to a [n] k char etc.

## code improvements

* scan for grapheme boundary is linear
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





