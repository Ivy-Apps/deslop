// Names the barrel by its directory form, which is how people write it.
// The chain to @/server/db runs through here and must not stop at the barrel.
import { greet } from "@/features/home";

export const page = () => greet();
