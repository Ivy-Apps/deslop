// A three-word name captured only in PascalCase, with the acronym in the
// middle. This Widget imports the wrong config, so the report spells out what
// the derived kebab-case actually is: 'DB' read as one word, the other two
// words kept apart. A passing case would have proved nothing, since a rule
// that silently failed to apply would look the same.
import { config } from '@/config/db-connection';

export function ArchiveDBOrderWidget() {
  return config.name;
}
