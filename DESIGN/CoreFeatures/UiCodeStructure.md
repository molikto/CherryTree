


## how UI code actually works

* editor
    * linked list renderer, renders: the order of node ids, nothing more. states: scroll position
    * node renderer
        * text renderer: render unfocused text
        * editor node: render selection mode and insert mode
        * line selection renderer
        
each component controls the rendering and manage the states, relayouting is automate
  
## how UI communicate with abstract model


there is two ways
1. UI wraps model, this way UI can get more accurate information about how and where the model changed
2. UI actions acts on model, then UI listens to model changes, as a pure function


## to React or not, that's a problem

I see no use of React (at least in the editor), because
1. using React with a editor is troublesome
2. most of our update is cheap (block reflow) or expensive (whole text relayout, you cannot pre-create spans, it is wrong in unicode condition), and we don't have a lot of level of control over it. so just home-make something that works