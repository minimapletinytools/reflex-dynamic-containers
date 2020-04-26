# reflex-dynamic-containers

Dynamic containers for reflex.

For usage examples, please see [todo-undo-mvc](https://github.com/pdlla/reflex-todo-undo-mvc-model).

Containers in this library have multiple update events. If any of these fire simultaneously, they will put out an stdout warning via `traceEvent` and all but one of the events are ignored by `leftmost`. This will almost certainly result in undesirable behavior.

PRs are very welcome.

These containers were written for use in [potato-flow](https://github.com/pdlla/potato-flow) (very WIP).
