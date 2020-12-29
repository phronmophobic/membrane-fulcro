# membrane-fulcro

A proof of concept using fulcro to build desktop and terminal applications

## Usage

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
