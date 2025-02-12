#!/usr/bin/env python3


def miguelsort(arr: list[int]) -> list[int]:
    bools: list[bool] = [False for _ in range(2**32)]

    for x in arr:
        bools[x] = True

    return [i for i, b in enumerate(bools) if b]


def main() -> None:
    arr = [4, 1, 6, 3, 9]
    sorted = miguelsort(arr)
    print(sorted)


if __name__ == "__main__":
    main()
