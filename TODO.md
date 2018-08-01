## roadmap

* code editor integration
    * the most simple will be just open a new dialog
    * no integration of vim keybindings
        * maybe these will have a seperate settings page...
    * no integration of undo/redo history, 
        * it's ok to let the editor handle themselves
        * after all change is done, or after sometime
          create a local edit ourselves by diffing, and consider
          this remote edit -- cannot undo outside
    * copy & paste
* global history
* conflicts??

## bugs

* movement in bidi text is wired, seems we have something wrong in our grapheme code
* when use j/k the cursor jumps too much scroll
* make menu scrollbar overlay


## improvements

* change to a better networking for change, server pushed content, or websocket

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





