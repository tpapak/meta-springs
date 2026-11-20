"""Local blog server with source-only change watching; no extra dependencies."""
import argparse
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import subprocess
import sys
import threading
import time
from urllib.parse import urlsplit

BLOG = Path(__file__).resolve().parent.parent


class LiveReloadHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def do_GET(self):
        if urlsplit(self.path).path == "/__blog_revision":
            self.send_bytes(str(self.server.revision).encode(), "text/plain")
            return
        path = Path(self.translate_path(self.path))
        if path.is_dir() and urlsplit(self.path).path.endswith("/"):
            path = path / "index.html"
        if path.is_file() and path.suffix.lower() == ".html":
            revision = self.server.revision
            try:
                page = path.read_bytes()
            except OSError:
                self.send_error(404)
                return
            script = ("<script>\n"
                      "(() => { const revision = '" + str(revision) + "';\n"
                      "async function poll() {\n"
                      "try { const response = await fetch('/__blog_revision', {cache: 'no-store'});\n"
                      "if (response.ok && (await response.text()).trim() !== revision) {\n"
                      "location.reload(); return; }\n"
                      "} catch (_) {}\n"
                      "setTimeout(poll, 1000); }\n"
                      "setTimeout(poll, 1000); })();\n"
                      "</script>\n").encode()
            position = page.lower().rfind(b"</body>")
            if position < 0:
                page += script
            else:
                page = page[:position] + script + page[position:]
            self.send_bytes(page, "text/html; charset=utf-8")
            return
        super().do_GET()

    def send_bytes(self, content, content_type):
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.wfile.write(content)

    def log_message(self, format, *args):
        if urlsplit(self.path).path != "/__blog_revision":
            super().log_message(format, *args)


def snapshot():
    paths = list((BLOG / "content").rglob("*.md"))
    paths += [p for p in BLOG.glob("*.html") if p.name != "index.html"]
    paths += list(BLOG.glob("*.js"))
    paths += [BLOG / "style.css", BLOG / "tools" / "mdblog.py"]
    result = {}
    for path in paths:
        try:
            result[str(path)] = path.read_bytes()
        except FileNotFoundError:
            pass  # A save can briefly replace a file.
    return result


def build():
    return subprocess.run(
        [sys.executable, str(BLOG / "tools" / "mdblog.py"), "build"],
        cwd=BLOG,
    ).returncode == 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("port", nargs="?", type=int, default=8000)
    args = parser.parse_args()
    handler = partial(LiveReloadHandler, directory=str(BLOG))
    try:
        server = ThreadingHTTPServer(("0.0.0.0", args.port), handler)
    except OSError as exc:
        parser.exit(1, f"Cannot start local server: {exc}\n"
                    "If another server is using this port, stop it or run: bash blog/dev.sh 8001\n")
    started = False
    try:
        previous = snapshot()
        if not build():
            return 1
        server.revision = time.time_ns()
        threading.Thread(target=server.serve_forever, daemon=True).start()
        started = True
        print(f"\nBlog: http://macas.lan:{server.server_port}/\n"
              "Live reload enabled. Save to rebuild and refresh automatically. Ctrl-C stops both.\n",
              flush=True)
        while True:
            time.sleep(1)
            current = snapshot()
            if current != previous:
                previous = current
                print("Source changed — rebuilding…", flush=True)
                if not build():
                    print("Build failed; fix the source and save to retry.", flush=True)
                else:
                    server.revision = time.time_ns()
    except KeyboardInterrupt:
        print("\nStopped.", flush=True)
    finally:
        if started:
            server.shutdown()
        server.server_close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
