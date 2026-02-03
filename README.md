# centaur-tabs-vertical

Vertical side tabs for centaur-tabs.

## MVP Scope

- Left and right side windows supported.
- Horizontal centaur-tabs are hidden while vertical mode is active.
- Icons are shown using centaur-tabs icon support.
- Mouse selection and middle-click close.
- Group list view (click to switch group).
- New tab button in the header.
- Drag the handle "|" to resize the sidebar width.

## Install (local)

```
(add-to-list 'load-path "/path/to/centaur-tabs/centaur-tabs-vertical")
(require 'centaur-tabs-vertical)
```

## Usage

```
(centaur-tabs-vertical-mode 1)
```

## Development (devenv)

```
devenv shell
test
```

Interactive:

- `M-x centaur-tabs-vertical-mode`
- `M-x centaur-tabs-vertical-enable`
- `M-x centaur-tabs-vertical-disable`

## Customization

```
;; Default: left only
(setq centaur-tabs-vertical-positions '(left))

;; Right side only
;; (setq centaur-tabs-vertical-positions '(right))

;; Both sides (same content on each side)
;; (setq centaur-tabs-vertical-positions '(left right))

(setq centaur-tabs-vertical-left-width 30)
(setq centaur-tabs-vertical-right-width 30)
(setq centaur-tabs-vertical-min-width 18)
(setq centaur-tabs-vertical-max-width 60)
(setq centaur-tabs-vertical-show-icons t)
(setq centaur-tabs-vertical-show-group t)
(setq centaur-tabs-vertical-show-group-list t)
(setq centaur-tabs-vertical-show-new-tab-button t)
```

## Keys (in the sidebar)

- `RET`: select tab
- `k`: close tab
- `g`: refresh
- `n`: new tab
- `s`: switch group (uses `centaur-tabs-switch-group`)

## Notes

This is an MVP. It intentionally focuses on core vertical tab behavior so we can iterate.
