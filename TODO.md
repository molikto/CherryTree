
## current constraint

* closed system - no over generalization, plugins
    * server part stays monolith, no generalization - there is not too much good of this yet
* no fancy UI performance tweaks for now. UI is the part that is easier to change
* complete information - offline does't work if you don't have all the information (no need to look at query tools like GraphQL)
*  we only support natural editing, i.e. the speed of content/information creation is proportional to the size of the document
    * so edits like copying a big trunk of data, then paste exactly the same, is not supported (can use pointers)
    

## top most (at most 5 tasks here)

* looks into https://github.com/facebook/yoga
* keyboard editor ui and data sync
    * make typing works
    * make cursor and selection in sync in text mode
* semantically data?
    * union type
    * reference type

## core

* references
* multi-sort node type: it is **never** ending story to abstract stuff
* branching
    * offline branch
* non-destructive manual conflict resolving
* abstract away Content class
* time traveling
* strings length is not well defined

## client


* keyboard and content editable
* mouse - I think I know what this does now, but consider now we only have a mouse-free experience
    * drag to move
    * text selection
    * node selection -- will handle like workflowy
* infinite scrolling -- currently we just use browser default
* settings: implement as also a document!
* rich text

## server

* possible split read and write?
* users and authentication
* db
* concurrency control
* user input validation
    * unique id collision detection
    * make logic all total, no exceptions

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
