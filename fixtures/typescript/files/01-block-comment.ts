/**
 * Calculates the nth Fibonacci number.
 * * @param n - Zero-based index.
 * @returns The Fibonacci number.
 */
import { validate } from "./utils";

/* Standard Block Comment:
   This part handles the iterative logic 
   without using recursion. */
function fib(n: number): number {
    // Single line comment with spaces
    if (n <= 1) return n;

    let a = 0, b = 1;
    for (let i = 2; i <= n; i++) {
        /* Inline block comment */
        let temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}