# Installation

Clone the repo and make sure you have the correct setup:
```
opam switch 4.02.3
opam install core yojson
eval `opam config env`
```

# Build

The makefile provides a default of building the `./ec` binary, so running
`make` should be all you need. You can also run the program with the example
task set in `./flashfill.json` using `make run`.

# Usage

The **`ec`** binary takes one argument: the filename of a json file. The
results are formatted in json and sent to stdout.

### Input
The input json must possess `grammar` and `tasks` fields which are lists.
Each item in the `grammar` list is an object with a single field, `expr`,
that points to a string representing a combinator. Each item in the `tasks`
list is an object with a single field, `problems`, that points to an array
of problems which must be satisfied by the same learned program. These
problems are objects with two string fields, `i` and `o` corresponding to
input and output for the program. Here's a simple example:
```json
{ "grammar": [
    { "expr":"(C nth)" },
    { "expr":"((B cap) lower)" }
  ],
  "tasks": [
    { "problems": [
      { "i":"test", "o":"tests" },
      { "i":"tree", "o":"trees" },
      { "i":"chair", "o":"chairs" }
    ]}
  ]
}
```

### Output
The output json has four fields: `grammar` (list), `programs` (list),
`log_bic` (float), and `hit_rate` (integer). Each item in the `grammar` list
is an object with two fields, `expr` (string) and `log_likelihood` (float),
corresponding to a string representation of a combinator and its associated
log-likelihood according to the given task set. Each item in the `programs`
list is an object with two fields, `task` (string) and `result` (null/obj),
corresponding to the task name and its results. `result` is null if the task
failed, or (on success) an object with `probability` (float) and `expr`
(string) fields, where `expr` is the string representation of the combinator
that solved the task and `probability` is its associated probability. Here's
a simple example:
```json
{
  "grammar": [
    { "expr": "(C nth)", "log_likelihood": -0.18777993926628667 },
    { "expr": "+1", "log_likelihood": -0.014358297145754317 },
    { "expr": "-1", "log_likelihood": -1.3729226430171515 },
    { "expr": "0", "log_likelihood": -0.004397513849555779 },
    { "expr": "B", "log_likelihood": -0.6763952213284927 },
    { "expr": "C", "log_likelihood": -0.46212867974216887 },
    { "expr": "I", "log_likelihood": -2.9097119180412037 },
    { "expr": "K", "log_likelihood": -2.225622460482148 },
    { "expr": "S", "log_likelihood": -1.8479804530847992 },
    { "expr": "feach", "log_likelihood": -0.0723850707542627 },
    { "expr": "nth", "log_likelihood": -0.7056911422330967 },
    { "expr": "fnth", "log_likelihood": -0.12211375457160134 }
  ],
  "programs": [
    { "task": "#include <os.h> -> OS", "result": null },
    {
      "task": "IaN -> Ian",
      "result": {
        "probability": 0.0010473575620008355,
        "expr": "((B cap) lower)"
      }
    },
    {
      "task": "IaN RoDny -> Ian Rodny",
      "result": {
        "probability": 1.416059563473085e-05,
        "expr": "((C feach) ((B cap) lower))"
      }
    }
  ],
  "log_bic": -4.77051e-8,
  "hit_rate": 6
}

```
