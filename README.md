A simple TODO application. Based on the concept of when things
are done, rather than when they are due. Things are stored in
a trivial single-file format.

## Adding things

Adding things is easy

```{.bash}
% todo-today 0 Read my email today
% todo-today 1 Go to the zoo
```

`.` is an alias for `0`.

The convention is you list the due date (not the date you plan to
work on something) in square brackets at the end of the task. 

```
% todo-today 1 Write TPS report [Aug 19th]
```

If you want a recurring task, put `,N` inside the due date.

```
% todo-today 1 Write TPS report [Aug 19th,7]
```

## Listing things

Without arguments, `todo-today` lists today's items. 
`todo-today` can also take either `today` or `show` as arguments,
which shows either just today, or everything.

```
% todo-today
 69        Today              : Start writing my ACM Queue article (Aug 9th)
 81        Today              : Read through the Proposal calls in TODO folder
% todo-today show
... all tasks, not just today ...
```

## Editing things

An editor can be used to edit the underlying file. This is just
an alias; the file can also be edited directly.

```
% todo-today edit
... brings up EDITOR ...
```

All tasks can be edited, and the code inside `[..]` is instructions what
to do to this task. The most common is `1`, which tells `todo-today`
to push this task back one day.

```
--| Today August 4th |------------------------------------
[1]    Remember the milk
```

The commands are:

  * [N] move a task back N days
  * [+]  is an alias for [1]
  * [D]  Done/completed. If recurring, the task is cloned, and the new task has
         a new due-date.
  * [X]  is an alias for [D]
  * [R]  Recurring done and cloned. The task cloned and moved to the next date;
         the do-date is also recomputed. The do-date is never set as after the due-date.
  * [A]  Abandoned (perhaps no longer relevant, someone else did it)

After the edit is complete, all codes are interpreted,
and a new version of TODO written. If you want to interpret
the commands without bringing up the editor, then you
can use `gc`.

```
% todo-today gc
4 tasks done, 5 tasked moved.
```

## Internals

`todo-today` looks for the environmental variable, TODO_TODAY,
which has a directory path in in. It defaults to the directory
`~/.todo`. This allows the tool to support multiple projects.

There are two files in this directory.

```
% ls ~/.todo
TODO
DONE
```

TODO is a set of active tasks; DONE is the tasks that 
have been done, or have been abandoned.

The file format is exactly as edited.

Lines that start with `--|` are computed, like dates.
Lines that start with `[` are tasks, one per lines.
Blank lines are allowed when read, but never written.

There are no other types of line.





