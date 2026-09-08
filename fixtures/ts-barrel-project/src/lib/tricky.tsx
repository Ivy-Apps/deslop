// Text that could swallow what follows it, followed by dependencies that must
// still be found. Every literal below must come back byte for byte, and every
// statement between them must still be reported and fixed.
const apostrophes = <p>don't stop, it's fine</p>;
const escaped = "he said \"hi\" and ended with a backslash \\";
const divided = a / b / c;
const regex = /['"]/;
const nested = `outer ${f(`inner ${g(`deep`)}`)} end`;
const notAStatement = `export * from "./nope";`;

// A real relative import, written after all of the above.
import { helper } from "./helper";

// A real relative re-export, written after all of the above.
export * from "./helper";

export const tricky = [apostrophes, escaped, divided, regex, nested, notAStatement, helper];
