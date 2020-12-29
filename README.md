# membrane-fulcro

A proof of concent of using fulcro to build Desktop and Terminal applications

## Installation

Download from https://github.com/com.phronemophobic/membrane-fulcro

## Usage

Run the project directly:

Run the desktop app:
    
```
$ clj -M -m com.phronemophobic.todo
```

![desktop todo](/membrane-fulcro-desktop-todo.gif?raw=true)


Run the terminal app:

```
;; don't use clj which uses rlwrap!
$ clojure -J -M -m com.phronemophobic.todo-terminal
```

![terminal todo](/membrane-fulcro-terminal-todo.gif?raw=true)


## License

Copyright © 2020 Adrian Smith

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
