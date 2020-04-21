# reflex-dynamic-containers

This library contains various dynamic containers for reflex.

Containers in this library have multiple updates events. If any of these fire simultaneously, you'll get an stdout warning via `traceEvent` and all but one of the events are ignored by `leftmost`. This will almost certainly result in undesirable behavior.

These containers were written for use in [potato-flow](https://github.com/pdlla/potato-flow) (very WIP) and therefore somewhat specific. PRs are very welcome.
