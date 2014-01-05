A simple TODO application. Based on the concept of when things
are done, rather than when they are due.

Adding things is easy

```
% todo-today +0 Read my email today
% todo-today +1 Task for tomorrow
```

Listing things is easy

```
% todo-today
 69        Today              : Start writing my ACM Queue article
 81        Today              : Read through the Proposal calls in TODO folder
% todo-today 81
 81        Today              : Read through the Proposal calls in TODO folder
% todo-today show
... all tasks, not just today ...
```

Bumping tasks to later is easy

```
% todo-today +1 81
```

Task 81 is now a task tomorrow

----

Adding a true todo date

```
% todo-today add 81 --by="2014-10-20"
```



