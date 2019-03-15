fun factorial(n: Int): Int { // declare name and type
    product = 1
    for (i = 2; i <= n; i = i + 1) { // runs for every i between 2 and n
        product = product * i
    }
    return product
}s
