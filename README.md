# reflex-dynamic-containers

Dynamic containers for reflex.

For usage examples, please see [todo-undo-mvc](https://github.com/pdlla/reflex-todo-undo-mvc-model).

Containers in this library have multiple update events. If any of these fire simultaneously, they will put out an stdout warning via `traceEvent` and all but one of the events are ignored by `leftmost`. This will almost certainly result in undesirable behavior.

Note, that it's unlikely that you would ever want to use these containers. It's much easier to apply `Event (Endo x)` to `Dynamic x` then splitting the inputs out into different events as in the containers here. This code is just an exercise and here for reference.

# TODOs
- switch to cabal only for better compatibility with rest of reflex ecosystem
- better documentation and examples
- more testing
