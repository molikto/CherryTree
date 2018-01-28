# events that changes state

## single user

in single user mode, there will be only one cause of a state change, that is: user input

### normal mode

in normal and visual mode, the state is only changed in selection

so what do we do? when enter these two mode, we construct a off-screen renderer, 
and on user state change events, we send them to the off-screen buffer, 
query the selection state changes, update local state, and render it (because we need to render our own cursor)

* alternative: change the actual node to content editable, but block all edits?
  the problem is how we hide the cursor
    * don't use this now if not for performance reasons, **no tricks**
* problem: is this expensive to construct content editable all the time?
  
### input mode

when user enter input mode, the cell itself is turned into a content editable
it is basically the same with normal mode, the state sync is also two way


## multi-user

in case of multi user, the changes can came from sources other than user input, so the diagram is like this:


```
/ remote user    ... ->    state /
                             |
                             \/
keyboard -> renderer    <=>  state
```


## all in one


### case 1

so some state changes is directly in state, like remote changes and events we handled ourselves in keyboard. 
in this case we will update the rendered result. notice that although normal mode selection will
query the renderer, it merely queries it, and the ui still need to be rendered from state

### case 2

in case of user input, the state goes to renderer, then we query the renderer to silently update the state. some cases 
we do need to update the renderer, we consider this a exit then an re-enter

### entering from case 1 to case 2

we need to sync the cursor position

