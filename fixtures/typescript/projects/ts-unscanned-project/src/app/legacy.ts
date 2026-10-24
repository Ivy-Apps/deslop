// Names the same unscanned file relatively. A relative specifier is a
// direction, not a name, so the report must not call the module by it - and
// '.' sorts below '@', so picking the smallest specifier reliably chose it.
import { client } from "../vendor/client";

export const legacy = () => client();
