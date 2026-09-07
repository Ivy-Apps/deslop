// Reached only through the barrel's aliased re-export. The alias runs the
// other way round from an import's - `formatTitle` is what this module
// exports, `title` is what a consumer of the barrel imports - and neither is
// the module, so the edge is unaffected either way.
import { audit } from "@/server/audit";

export type HomeProps = { title: string };

export const formatTitle = (props: HomeProps): string => audit(props.title);
