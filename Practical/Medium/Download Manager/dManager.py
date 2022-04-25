import click
import requests
import threading


def Handler(start, end, url, filename):
    headers = {"Range": "bytes=%d-%d" % (start, end)}
    r = requests.get(url, headers=headers, stream=True)
    with open(filename, "r+b") as f:
        f.seek(start)
        f.tell()
        f.write(r.content)


@click.command(help="Download a file in multiple threads")
@click.option("--number_of_threads", default=4, help="Number of threads to use")
@click.option("--name", type=click.Path(), help="Name of the file to download")
@click.argument("url", type=click.Path(), required=True)
@click.pass_context
def download(number_of_threads, name, url):
    r = requests.head(url)
    if name:
        file_name = name
    else:
        file_name = url.split("/")[-1]
    try:
        file_size = int(r.headers["Content-Length"])
    except KeyError:
        print("Cannot get file size")
        return

    part_size = int(file_size) // number_of_threads
    fp = open(file_name, "wb")
    fp.truncate(file_size)
    fp.write(b"\0" * file_size)
    fp.close()

    for i in range(number_of_threads):
        start = part_size * i
        end = part_size * (i + 1)
        if i == number_of_threads - 1:
            end = file_size
        t = threading.Thread(target=Handler, args=(start, end, url, file_name))
        t.setDaemon(True)
        t.start()

    main = threading.currentThread()
    for t in threading.enumerate():
        if t is not main:
            t.join()
    print("Download complete")


download(obj={})
