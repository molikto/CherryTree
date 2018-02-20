# what is our state

tree of (b[], metadata)

selections state of different mode

where metadata contains logic in `plslang`

* normal: is [a, b), because.. a normal mode is basically a selection
* insert: is a one value [.., b)
* visual: like normal
* visual line: simple


a composition event start will actually block the whole state update cycle. this is a good choice
