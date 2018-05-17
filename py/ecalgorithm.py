import os.path, subprocess, json

_ROOT = os.path.abspath(os.path.dirname(__file__))
ec_path = os.path.join(_ROOT, 'ec_str')

def string_transformations():
    global ec_path
    ec_path = os.path.join(_ROOT, 'ec_str')

def list_functions():
    global ec_path
    ec_path = os.path.join(_ROOT, 'ec_list')

class Task:
    def __init__(self, name, tp, examples):
        """
        :param name: string
        :param tp: string denoting the type for a task solution
        :param examples: list of IO duples
        """
        self.name = name
        self.examples = examples
    def objectify(self):
        examples = list(map(lambda t:{"i":t[0],"o":t[1]}, self.examples))
        return {"name": self.name,
                "examples": examples }

def run(tasks, frontier_size=5000, it=5, lambd=1.5, smoothing=1.0):
    """
    :param tasks: a list of Task
    :returns: a list of dicts {task, result} where the value of "task" is the
        name of the task, and "result" is {log_probability, expr, time} or None
        if no matching program was discovered
    """
    tasks = list(map(Task.objectify, tasks))
    p = subprocess.Popen([ec_path, "-",
        "-frontier-size", str(frontier_size),
        "-it", str(it),
        "-lambda", str(lambd),
        "-smoothing", str(smoothing),
        ],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    p.stdin.write(bytes(json.dumps({"grammar": [], "tasks": tasks}), 'utf-8'))
    return json.loads(p.communicate()[0])
