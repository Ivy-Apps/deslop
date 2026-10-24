// Reached only through the barrel's type-only re-export. It erases at runtime,
// but an architecture rule is about what a module depends on, so it is an edge.
import type { Row } from "@/server/schema";

export type HomeProps = { row: Row };
