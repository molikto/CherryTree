


our data is a tree

## data

each tree has a unique `id: Id`, and it is the id of that node

## pointers


* `Pointer`
   * absolute pointer `{id: Id}`
   * relative pointer `{pointer: Pointer, relative: Movement}`
   * unbox pointer `{pointer: Pointer}`, where pointer points to a pointer.., where pointer points to a pointer...

## a movement is a sequence of 

* parent
* left
* right
* first child
* last child
* *follow pointer in content somehow?*
