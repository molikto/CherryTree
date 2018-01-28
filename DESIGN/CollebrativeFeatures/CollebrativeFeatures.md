


ideas you can take from:


* IntelliJ IDEA works under source control, we always have a 
local editing history, and other people's edits is considered 
a external edit, when going back, we go back in the global editing tree
    * dialog: external changes, confirm to undo
    * this is bad in a collaborative environment

* a editing is:
    * block level
        * create new empty child
        * delete child
        * move child
        * change meta data
    * content level
        * add str
        * remove str
    * a composite edit
        * most of time, a composite edit is produced from things like a copy paste
* we consider document state $g$, an editing 
    * each editing produce a map of $e: g \to g'$, a reverse edit $e-1: g' \to g$
    * all edits, including all remote edits, is queued as a list
    * each time we do a redo, we try to commute two edits
    
* local selection
    * removal can remove local focus completely
        * in this case, a saner choice is move users' cursor to parent node if it is block level, or move it to first not deleted node it if is text level
    
    
* local viewport
    * moving stuff out of viewport not necessary create a edit conflict. but it might end current editing sequence, and notice the user that "the part of doc you are editing is moved to ...."
   
* so, notifications
    * so our model is:
        * we don't make user move out of the viewport
        * we might interrupt current editing sequence if necessary
        * we post error message and pointers on things user should notice
        * for undo sequence, we might change the viewport
        * cursor change might be in the undo sequence, this is IntelliJ IDEA's behavior, and both feels ok
        
        
* how to represent other's editing cursor
    * in a WYSWYE editor, it is easy. but in case user is editing inline LaTeX, then it is NOT easy. what you need is a mapping from 
    editing sequence to display sequence, and they might be a very different form. like inline bold blocks and inline LaTeX
    firstly, we need a markdown parser, i.e. the abstract renderer, it renders the textual data into a 
    tree, with annotations of "line numbers", like in a code transformer
    * there are cases where selection in textual form is invalid in tree form, like selecting sibling and a partial child.
    in this case, we might have a unfaithful selection, and we can visually represent it as a different color??
    * what if the mapping is not continuous?
    * also the selection highlighting is vastly different then. because we want to select formatted text
    * the simplest way is to represent the cursor only at block level
    * mmmm.... **the actual solution is **show the cursor when user is also focused, show only the head when user is not focused**
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
