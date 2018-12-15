# my-own-language
This is a translator from designed by myself imperative language to C

Sample program:

```c++
fun fib(n: int) : int {
    if (n < 2) {
        ret 1;
    } else {
        ret fib(n - 1) + fib(n - 2);
    }
}

fun print_fibs(n: int) : void {
    for (i : int = 0; i < n; i += 1) {
        printf("fib #%d = %d\n", i, fib(i));
    }
}

print_fibs(10);


arr : int[10];

fun fill_glob_arr() : void {
    for (i: int = 0; i < 10; i += 1) {
        arr[i] = rand() mod 100;
    }
}

fun sort_glob_arr() : void {
    for (i: int = 0; i < 10; i += 1) {
        for (j: int = 0; j < 10 - 1 - i; j += 1) {
            if (arr[j] > arr[j+1]) {
                t: int = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = t;
            }
        }
    }
}

fun print_glob_arr() : void {
    for (i: int = 0; i < 10; i += 1) {
        printf("arr[%d] = %d\n", i, arr[i]);
    }
    printf("~~~~~~~~~~~\n");
}

fill_glob_arr();
print_glob_arr();
sort_glob_arr();
print_glob_arr();
```
