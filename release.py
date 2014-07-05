#!/usr/bin/env python
"""This script generates release notes for each merged pull request from
git merge-commit messages.

Usage:

 `python release.py <start_commit> <end_commit> [--output {file,stdout}]`

For example, if you wanted to find the diff between version 1.0 and 1.2,
and write the output to the release notes file, you would type the
following:

 `python release.py 1.0 1.2 -o file`

"""
import os.path as op
import re
import subprocess
from collections import deque


def commit_msgs(start_commit, end_commit):
    """Run the git command that outputs the merge commits (both subject
    and body) to stdout, and return the output.

    """
    fmt_string = ("'%s%n* [#{pr_num}]"
                  "(https://github.com/elixir-lang/emacs-elixir/pull/{pr_num}) - %b'")
    return subprocess.check_output([
        "git",
        "log",
        "--pretty=format:%s" % fmt_string,
        "--merges", "%s..%s" % (start_commit, end_commit)])


def release_note_lines(msgs):
    """Parse the lines from git output and format the strings using the
    pull request number.

    """
    ptn = r"Merge pull request #(\d+).*\n([^\n]*)'$"
    pairs = re.findall(ptn, msgs, re.MULTILINE)
    return deque(body.format(pr_num=pr_num) for pr_num, body in pairs)


def release_header_line(version, release_date=None):
    release_date = release_date or datetime.date.today().strftime('%Y/%m/%d')
    return "## %s - %s" % (version, release_date)


def prepend(filename, lines, release_header=False):
    """Write `lines` (i.e. release notes) to file `filename`."""
    if op.exists(filename):
        with open(filename, 'r+') as f:
            first_line = f.read()
            f.seek(0, 0)
            f.write('\n\n'.join([lines, first_line]))
    else:
        with open(filename, 'w') as f:
            f.write(lines)
            f.write('\n')


if __name__ == "__main__":
    import argparse
    import datetime

    parser = argparse.ArgumentParser()
    parser.add_argument('start_commit', metavar='START_COMMIT_OR_TAG')
    parser.add_argument('end_commit', metavar='END_COMMIT_OR_TAG')
    parser.add_argument('--filepath', '-f',
                        help="Absolute path to output file.")
    parser.add_argument('--tag', '-t', metavar='NEW_TAG')
    parser.add_argument(
        '--date', '-d', metavar='RELEASE_DATE',
        help="Date of release for listed patch notes. Use yyyy/mm/dd format.")
    args = parser.parse_args()
    start, end = args.start_commit, args.end_commit
    lines = release_note_lines(commit_msgs(start, end))

    if args.tag:
        lines.appendleft(release_header_line(args.tag, args.date))

    lines = '\n'.join(lines)

    if args.filepath:
        filename = op.abspath(args.filepath)
        prepend(filename, lines)
    else:
        print lines
