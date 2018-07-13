

## current constraint

* closed system - no over generalization, plugins
    * server part stays monolith, no generalization - there is not too much good of this yet
* no fancy UI performance tweaks for now. UI is the part that is easier to change
* complete information - offline does't work if you don't have all the information (no need to look at query tools like GraphQL)
*  we only support natural editing, i.e. the speed of content/information creation is proportional to the size of the document
    * so edits like copying a big trunk of data, then paste exactly the same, is not supported (can use pointers)
    

## now

* commands and vim base structure
* keyboard editor for paragraph
* mouse support
* copy paste
* drag drop
* search and replace

## server

* possible split read and write?
* users and authentication
* db
* concurrency control
* user input validation
    * unique id collision detection
    * make logic all total, no exceptions
    
## future

* reference type
* tables (the most important thing is how to create a simple keyboard interface to create, select and edit)
* data records (like book entry)
* branching
    * offline branch
* time traveling
* scripting


## good to have

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
* ~~paragraph view present~~
* ~~mode definition is kinda of wrong? -- not wrong, we are sticking with the linearized formation for now~~
