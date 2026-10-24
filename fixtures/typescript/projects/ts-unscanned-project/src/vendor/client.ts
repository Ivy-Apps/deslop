// Gitignored: Deslop resolves imports of this file but never scans it, so the
// chain through it stops here. Its own dependency on @/server/secret is
// invisible to every rule.
import { secret } from "@/server/secret";

export const client = () => secret();
