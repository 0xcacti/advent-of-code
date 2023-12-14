from itertools import chain

def find_mirrors(pattern):
    # Check vertical
    ncol = len(pattern[0])
    for i in range(1, ncol):
        mirrored = True
        for l in pattern:
            nm = min(i, ncol - i)
            left = l[i-nm:i]
            right = l[i:i+nm]
            if left != right[::-1]:
                mirrored = False
                break
        if mirrored:
            return i

    # Check horizontal
    nrow = len(pattern)
    for i in range(1, nrow):
        mirrored = True
        nm = min(i, nrow - i)
        top = pattern[i-nm:i]
        bot = pattern[i:i+nm][::-1]
        if any(lt != lb for lt, lb in zip(top, bot)):
            mirrored = False
            continue
        if mirrored:
            return 100 * i

    print("\n".join(pattern))
    raise ValueError("No line of reflection found")


def day13_part1(filename):
    with open(filename) as f:
        lines = f.read()
        mirrors = lines.split("\n\n")
        mirrors = list(map(lambda s: s.split("\n"), mirrors))
        mirrors[-1] = mirrors[-1][:-1] #  remove trailing newline
    
    for m in mirrors:
        printMirror(m)
        print(find_mirrors(m))
    return sum(find_mirrors(m) for m in mirrors)

def printMirror(m):
    print("\n".join(m))


if __name__ == "__main__":
    print("Part 1", day13_part1("input.txt"))

