// The same barrel, named by its index form. Both spellings are one module,
// so this must be reported exactly as page.ts is.
import { greet } from "@/features/home/index";

export const explicit = () => greet();
