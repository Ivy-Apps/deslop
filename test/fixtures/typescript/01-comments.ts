/**
 * Calculates the nth Fibonacci number using an iterative approach.
 * The sequence is defined as: F(0)=0, F(1)=1, F(n) = F(n-1) + F(n-2).
 *
 * @param n - The zero-based index in the Fibonacci sequence to retrieve.
 * @returns The Fibonacci number at index n.
 * @throws {Error} If n is a negative integer.
 */
function calculateFibonacci(n: number): number {
    // Step 1: Input Validation.
    // Check if the input 'n' is less than zero. The Fibonacci sequence starts at index 0.
    if (n < 0) {
        // If the input is negative, throw an error indicating invalid input.
        throw new Error("Fibonacci index must be a non-negative integer.");
    }

    // Step 2: Handle Base Case 0.
    // If n is exactly 0, the result is implicitly defined as 0.
    if (n === 0) {
        // Return 0 and exit the function immediately.
        return 0;
    }

    // Step 3: Handle Base Case 1.
    // If n is exactly 1, the result is implicitly defined as 1.
    if (n === 1) {
        // Return 1 and exit the function immediately.
        return 1;
    }

    // Step 4: Initialize State Variables for Iteration.
    // We need to keep track of the two preceding numbers to calculate the next one.

    // 'prevPrev' represents F(i-2). We initialize it to F(0), which is 0.
    let prevPrev: number = 0;

    // 'prev' represents F(i-1). We initialize it to F(1), which is 1.
    let prev: number = 1;

    // 'current' will store the result of F(i) during each iteration.
    // We initialize it to 1 (the value for n=1) as a starting point before the loop begins.
    let current: number = 1;

    // Step 5: Begin the Iterative Loop.
    // We start the loop at index i = 2 because indexes 0 and 1 were handled by base cases.
    // The loop runs as long as 'i' is less than or equal to the target index 'n'.
    for (let i = 2; i <= n; i++) {
        // Step 5a: Calculate the next Fibonacci number.
        // Add the value two steps back (prevPrev) to the value one step back (prev).
        current = prevPrev + prev;

        // Step 5b: Shift variables to prepare for the next iteration cycle.
        // The value that was one step back becomes the value two steps back for the next turn.
        prevPrev = prev;

        // The value we just calculated as current becomes the value one step back for the next turn.
        prev = current;
        // End of loop iteration. The loop counter 'i' will increment now.
    }

    // Step 6: Return Result.
    // Once the loop has finished running until i=n, the variable 'current' holds the correct Fibonacci number.
    // Return the final calculated value.
    return current;
}