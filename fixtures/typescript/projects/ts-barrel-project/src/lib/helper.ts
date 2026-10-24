// Written with the second alias onto the same directory. It resolves to the
// very same file @/server/db names, so both spellings are one module.
import { query } from "~/server/db";

export const helper = () => query();
