"""

regex to find function names in source:
(?<=\n)\w+(?=[^=:\n]* *=)


exported:
(?:module \w+\s+exposing\s+)(\(([^\)\(]*|\([^\)]*\))*\))

types:
(?:type\s+)\w+

constructors:


remove everything we cannot find; we might get matches in block comments etc. and also false matches from bad regex

"""

"""
list of things we aren't handling:

- missing module definition
- module definition with name other than filename
- putting imports at the top of the file
- removing empty tuples () (but not when they're used as the unit type, e.g. lambdas)

"""
import os

import re


# TODO: handle exported types

def find_functions(s):
    pattern = re.compile(r"(?<=\n)\w+(?=[^=:\n]* *=)")
    matches = pattern.findall(s)
    return list(matches)


def find_types(s):
    pattern = re.compile(r"(?:type\s+)\w+")
    matches = pattern.findall(s)
    matches = [x.replace("type", "").strip() for x in matches]
    return list(matches)


def find_exported_things(s):
    pattern = re.compile(r"(?:module \w+\s+exposing\s+)\(((?:[^)(]*|\(..\))*)\)")
    matches = pattern.findall(s)
    things = list(matches)
    if things == [".."]:
        return ".."
    if not things:
        return []
    things = things[0].split(",")
    things = [x.strip() for x in things]
    return things


###

def replace_exposing(s, found_functions):
    exposing_pos = 0
    for x in s.splitlines(keepends=True):
        exposing_pos += len(x)
        if x.strip().endswith("exposing"):
            break
    module_pos = 0
    for x in s.splitlines(keepends=True):
        if x.strip() == "":
            break
        module_pos += len(x)

    exposing, module, code = s[:exposing_pos], s[exposing_pos:module_pos], s[module_pos:]

    exported = [x.strip().strip("()").strip() for x in module.split(",")]

    cleaned_module_list = [x for x in exported if x in found_functions]

    # build new s
    new_s = exposing
    new_s += "    (" + ", ".join(cleaned_module_list or [".."]) + ")"
    new_s += "\n"
    new_s += code

    return new_s


# TODO: next up, replace module exposing, removing anything that's missing

if __name__ == "__main__":
    for root, dirs, files in os.walk('.'):
        if "elm-stuff" in root:
            continue
        for file in files:
            file_path = os.path.join(root, file)
            if file.endswith(".elm"):
                print("elm", file_path)
                with open(file_path, "r+") as f:
                    s = f.read()

                    # remove line comments in input
                    s = re.sub(r"^\s*-- .*\n", "", s, flags=re.MULTILINE)

                    # remove empty "exposing" (in import statements)
                    s = s.replace(" exposing ()", "")

                    functions = find_functions(s) + find_types(s)
                    exported = find_exported_things(s)
                    # print("functions", functions)
                    # print("exported", exported)
                    private = []
                    if exported != "..":
                        # private = [x for x in functions if x not in exported]
                        # print("private", private)

                        missing = []
                        missing = [x for x in exported if x not in functions]
                        if missing:
                            print("missing", missing)

                        # replace exposing
                        new_s = replace_exposing(s, functions)

                        f.seek(0)
                        f.write(new_s)
                        f.truncate()
