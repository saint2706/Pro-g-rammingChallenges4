import sys
import datetime


def help():
    helpstring = """How to Use :-
$ ./todo add "item"     # Add a new item to the to do list
$ ./todo show           # Show remaining todos
$ ./todo delete "id"    # Delete a todo
$ ./todo fin "id"       # Complete a todo
$ ./todo help           # Show usage
$ ./todo stats          # Statistics"""
    sys.stdout.buffer.write(helpstring.encode("utf8"))


# general utility
def test():
    try:
        f = open("todo.txt", "r")
        c = 1
        for line in f:
            line = line.strip("\n")
            d.update({c: line})
            c += 1
    except:
        sys.stdout.buffer.write("There are no pending todos!".encode("utf8"))


def add(item):
    with open("todo.txt", "a") as f:
        f.write(item)
        f.write("\n")
    item = '"' + item + '"'
    print(f"Added {item}")


def show():
    try:

        test()
        l = len(d)
        for _ in d:
            sys.stdout.buffer.write(f"[{l}]-{d[l]}".encode("utf8"))
            sys.stdout.buffer.write("\n".encode("utf8"))
            l -= 1

    except Exception as e:
        raise e


def delete(id):
    try:
        id = int(id)
        test()
        with open("todo.txt", "r+") as f:
            lines = f.readlines()
            f.seek(0)
            for i in lines:
                if i.strip("\n") != d[id]:
                    f.write(i)
            f.truncate()
        print(f"Removed #{id}")

    except Exception as e:
        print(f"#{id} does not exist. Nothing deleted.")


def done(id):
    try:

        test()
        id = int(id)
        with open("done.txt", "a") as f:
            st = "x " + str(datetime.datetime.today()).split()[0] + " " + d[id]
            f.write(st)
            f.write("\n")
        print(f"Completed #{id}.")

        with open("todo.txt", "r+") as f:
            lines = f.readlines()
            f.seek(0)
            for i in lines:
                if i.strip("\n") != d[id]:
                    f.write(i)
            f.truncate()
    except:
        print(f"#{id} does not exist.")


def report():
    test()
    try:

        f = open("done.txt", "r")
        c = 1
        for line in f:
            line = line.strip("\n")
            done.update({c: line})
            c += 1
        print(
            f"{str(datetime.datetime.today()).split()[0]} ToDo : {len(d)} Completed : {len(done)}"
        )
    except:
        print(
            f"{str(datetime.datetime.today()).split()[0]} ToDo : {len(d)} Completed : {len(done)}"
        )


if __name__ == "__main__":
    try:
        d = {}
        done = {}
        args = sys.argv
        if args[1] == "add" and not len(args[2:]):
            sys.stdout.buffer.write(
                "Error: Missing todo string. Nothing added!".encode("utf8")
            )

        elif args[1] == "done" and not len(args[2:]):
            sys.stdout.buffer.write(
                "Error: Missing id for marking todo as done.".encode("utf8")
            )

        elif args[1] == "delete" and not len(args[2:]):
            sys.stdout.buffer.write(
                "Error: Missing NUMBER for deleteeting todo.".encode("utf8")
            )
        else:
            globals()[args[1]](*args[2:])

    except Exception as e:

        hs = """How to Use :-
$ ./todo add "item"     # Add a new item to the to do list
$ ./todo show           # Show remaining todos
$ ./todo delete "id"    # Delete a todo
$ ./todo fin "id"       # Complete a todo
$ ./todo help           # Show usage
$ ./todo stats          # Statistics"""
        sys.stdout.buffer.write(hs.encode("utf8"))