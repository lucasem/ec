# Installation

Clone the repo and make sure you have the correct setup:
```
opam switch 4.06.0
eval `opam config env`
opam install core.v0.9.2 re2.v0.9.1 yojson
```

# Build

The makefile provides a default of building both the `./str` binary for
**string transformations** and the `./list` binary for **list functions**.
Running `make` should be all you need.

You can also run the program with the example string transformation task set
in `./flashfill.json` using `make run`, or by following the usage explained
below.

# Usage (command-line)

The **`ec`** binary takes one argument: the filename of a json file. The
results are formatted in json and sent to stdout. If the filename is `-`,
stdin is used to read the json input.

### Settings

The following optional flags are provided:
```
[-frontier-size integer]  Frontier size (default: 5000)
[-it integer]             Number of iterations (default: 5)
[-lambda float]           Grammar learning cutoff (lower ~ more eager
                          learning) (default: 1.5)
[-smoothing float]        Grammar parameter estimation factor (lower ~ more
                          eager learning) (default: 1.0)
```

These flags may be used as follows:
```sh
./ec -it 7 -frontier-size 10000 -lambda 0.6 -smoothing 1.0 flashfill.json
```

### Input
The input json must possess `grammar` and `tasks` fields which are lists.
Each item in the `grammar` list is an object with a single field, `expr`,
that points to a string representing a combinator. Each item in the `tasks`
list is an object with three fields: a unique `name`, a `type`, and
`examples` which points to an array of problems which must be satisfied by
the same learned program. These problems are objects with two fields, `i`
and `o` corresponding to input and output for the program. Here's a simple
example:
```json
{ "grammar": [
    { "expr":"(C nth)" },
    { "expr":"((B cap) lower)" } ],
  "tasks": [
    { "name": "append s",
      "type": "string -> string",
      "examples": [
        { "i":"test", "o":"tests" },
        { "i":"tree", "o":"trees" },
        { "i":"chair", "o":"chairs" } ] }
  ]
}
```

### Output
The output json has four fields: `grammar` (list), `programs` (list), and
`hit_rate` (integer). Each item in the `grammar` list is an object with two
fields, `expr` (string) and `log_likelihood` (float), corresponding to a
string representation of a combinator and its associated log-likelihood
according to the given task set. Each item in the `programs` list is an
object with two fields, `task` (string) and `result` (null/obj),
corresponding to the task name and its results. `result` is null if the task
failed, or (on success) an object with `log_probability` (float), `expr`
(string), and `time` (float) fields, where `expr` is the string
representation of the combinator that solved the task and
`log_probability` is its associated log-probability, and `time` is how
long, in seconds, it took to enumerate the solution in the last iteration
of the EC algorithm.  Here's a simple example:

```json
{
  "grammar": [
    { "expr": "((B cap) lower)", "log_likelihood": -0.18777993926628667 },
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
        "log_probability": -6.86148489441,
        "expr": "((B cap) lower)",
        "time": 0.056375980377197266
      }
    },
    {
      "task": "IaN RoDny -> Ian Rodny",
      "result": {
        "log_probability": -11.165047406,
        "expr": "((C feach) ((B cap) lower))",
        "time": 0.056375980377197266
      }
    }
  ],
  "hit_rate": 2
}
```

# Usage (python)

To install the python package, you must be using python 3.
Clone this repository and execute the following:

```bash
$ pip install -e ec/py
```

This gives you the `ecalgorithm` python module.

```py
import ecalgorithm as ec

# set the appropriate EC binary:
ec.string_transformations()
#ec.list_functions()

tasks = [
    ec.Task("upper first", "string -> string", [
        ("some input", "SOME input"),
        ("yet another example", "YET another example")
    ]),
    ec.Task("IaN to Ian", "string -> string", [
        ("IaN", "Ian"),
        ("MELVIN", "Melvin"),
    ]),
]

# outputs a python dict corresponding to the
# JSON output from command-line usage
output = ec.run(tasks,
    frontier_size=5000,
    it=3,
    lambd=1.0,
    smoothing=1.0,
)

for t in output["programs"]:
    name = t["task"]
    if t["result"] is None:
        print(f"task {name} failed")
        continue
    expr = t["result"]["expr"] # string of combinatory logic expression
    print(f"task {name} solution: {expr}")

"""stdout:
task IaN to Ian solution: ((B cap) lower)
task upper first solution: ((fnth upper) 0)
"""
```

# Misc

### Types

Supplied types are delimited by `->` and can be `int`, `bool`, `char`,
`string`, or those primitive types prefixed with `list-of-`.

### Mini

A convience build `make mini`, corresponding to [`./mini.ml`](./mini.ml),
helps to quickly test and use expressions.
