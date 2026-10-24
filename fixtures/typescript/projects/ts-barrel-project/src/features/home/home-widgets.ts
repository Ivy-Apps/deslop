// Reached only through the barrel's `export * as` re-export.
import { cache } from "@/server/cache";

export const widget = () => cache();
