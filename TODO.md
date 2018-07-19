

## current constraint

* closed system - no over generalization, plugins
    * server part stays monolith, no generalization - there is not too much good of this yet
* no fancy UI performance tweaks for now. UI is the part that is easier to change
* complete information - offline does't work if you don't have all the information (no need to look at query tools like GraphQL)
*  we only support natural editing, i.e. the speed of content/information creation is proportional to the size of the document
    * so edits like copying a big trunk of data, then paste exactly the same, is not supported (can use pointers)
    

## now

*the vim commands currently is limited to what is implemented in Vimflowy, a seperate future project will look into implement more*

* easy motion
* multi char find/change
* closing node
* modal editor for link like
* modal editor for LaTeX
* rebase move
* node type
* replace mode
* insert image?
* available commands panel
* single edit commands
* copy paste
* code mirror!!!
* insert mode command overrides
* make so that the state exposed to view is the partial one
* sync external operation to content
* keyboard editor for rich
* mouse support
* clicking
* drag drop
* search and replace
* line motions? they are actually pretty useful?

## server

* possible split read and write?
* users and authentication
* db
* concurrency control
* user input validation
    * unique id collision detection
    * make logic all total, no exceptions
    
## future

* read though a vim reference and implement missing features that can be ported
* reference type
* tables (the most important thing is how to create a simple keyboard interface to create, select and edit)
* data records (like book entry)
* branching
    * offline branch
* time traveling
* scripting


## good to have

* find a method to disable Chinese input in normal/visual mode, also dictation on Mac, i.e. different selection rendering
* patch chrome/electron so we get a better looking selection
* macros
* theme system
* multi-tabs
* register for copy paste
* outline panel for quick navigation
* infinite scrolling -- currently we just use browser default
* make it embeddable, currently we assume it is a standalone thing
* compatible with other browsers, currently only recent version of Chrome is supported
* because we don't have reference links, a good start is if a link is modified, we try to change all links with same description and url and tell the user, something like this
* exc to exit editing panel?
* char counter
* support or disable print (currently print with control glyphs)
* importers/exporters


## build

* production build?

## open system?

* a big big open problem...
    * communication protocol can be made dynamic, but storage most of time cannot, also c/s plugin is hard to maintain

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
